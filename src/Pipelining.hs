{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Pipelining (Pipeline, collectBuffers, convertPls) where

-- The idea is to transform a list of shuffles and maps into a list of maps (containing array)

import Code
import Control.Lens
import Control.Monad
import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Language.GaiwanDefs

data PipelineStep = PipelineStep
  { _outBuf :: [GPUBuffer],
    _expValue :: [Exp]
  }
  deriving (Show)

makeLenses ''PipelineStep

data Pipeline = Pipeline
  { _shuffle :: Maybe [(GPUBuffer, Exp)], -- Shuffle to apply to the _expValue of the _curExp → Sets the index to write to in the specified buffer?
    _curExp :: PipelineStep, -- TODO: should be a LIST
    _doneExps :: [PipelineStep]
    -- _curSize :: Int -- todo: what is the point of this field?
  }
  deriving (Show)

makeLenses ''Pipeline

-- Normalize the Exp (always use array0, array1, ...)
convertPipe :: [Exp] -> SCode [PipelineStep]
convertPipe (h : r) = reverse . dataToList <$> foldl convP (convH h) r
  where
    -- convert first item of the pipe
    convH :: Exp -> SCode Pipeline
    convH (App "generateSeq" True [Int count, Int n]) = do
      buffers <- replicateM count $ freshGPUBuffer n
      return $ emptyData buffers
    convH _ = error "Unknown start sequence"

-- converts subsequent steps
-- (we need the SCode State monad to translate names into calls to the right kind of steps)
convP :: SCode Pipeline -> Exp -> SCode Pipeline
convP x a@Loop {} = convPLoop x a
convP x a@(App "split" True [Int numBuf, Int offset]) | numBuf > 0 && offset > 0 = convPSplitter numBuf offset <$> x
convP x a@(App "join" True [Int numBuf, Int offset]) | numBuf > 0 && offset > 0 = x >>= convPJoin numBuf offset
convP x a@(App name True _) = error $ "Unknown builtin function name " ++ name
convP x a@(App n False _) = do
  t <- lookupDef n
  y <- x
  case t of
    Just f@Shuffler {} -> return $ convPShuffler y f a
    Just f@Mapper {} -> convPMapper y f a
    _ -> error $ "Unknown name " ++ show n

convPJoin :: Int -> Int -> Pipeline -> SCode Pipeline
convPJoin numBuf offset x = do
  let groupedBuffers = groupByCnt $ shuffledOutBuf x
  freshBuffers <- mapM (\((GPUBuffer _ s, _) : _) -> freshGPUBuffer s) groupedBuffers -- todo check if same buffer (use sum of sizes)
  return $
    x
      & curExp
        .~ PipelineStep
          { _outBuf = freshBuffers, -- Out is new buffer
            _expValue =
              map
                ( convToIf
                    . zipWith
                      indexPos
                      [0 ..] -- bufferIndex
                )
                groupedBuffers
          }
      & doneExps %~ ((x ^. curExp) :) -- prepend the old curExp
      & shuffle .~ Nothing
  where
    chunkSize = offset * numBuf
    indexPos :: Int -> (GPUBuffer, Exp) -> (Int, Exp)
    indexPos bufferIndex (buffer, sufffle) =
      ( bufferIndex,
        gpuBufferGet buffer $
          Plus
            (Times (Div indexVar (Int chunkSize)) (Int offset))
            (Modulo indexVar (Int offset))
      )

    groupByCnt :: [a] -> [[a]]
    groupByCnt [] = []
    groupByCnt l | length l >= numBuf = let (h, rest) = splitAt numBuf l in h : groupByCnt rest
    groupByCnt l = error $ "could not join list that is not a multiple of the arg, left: " ++ show (length l)
    convToIf :: [(Int, Exp)] -> Exp
    convToIf [(_, e)] = e
    convToIf ((bufferIndex, e) : rest) =
      If
        ( IsEq
            (Modulo (Div indexVar (Int offset)) (Int numBuf))
            (Int bufferIndex)
        )
        e
        (convToIf rest)

convPSplitter :: Int -> Int -> Pipeline -> Pipeline
convPSplitter numBuf offset x =
  x
    & shuffle
      ?~ concatMap
        ( \(buf, shuff) ->
            map
              ( \bufferNum ->
                  ( buf,
                    subst indexVar (indexPos bufferNum) shuff
                  )
              )
              [0 .. (numBuf -1)]
        )
        (shuffledOutBuf x)
  where
    indexPos :: Int -> Exp
    indexPos bufferNum =
      Plus
        (Times (Div indexVar offsetE) (Int $ numBuf * offset))
        ( Plus
            (Int $ bufferNum * offset)
            (Modulo indexVar offsetE)
        )
    offsetE = Int offset

shuffledOutBuf :: Pipeline -> [(GPUBuffer, Exp)]
shuffledOutBuf x = fromMaybe (emptySuffle (x ^. (curExp . outBuf))) $ x ^. shuffle

-- A shuffle is stored in a variable for later reference, when we actually need it.
-- A shuffler has following args :
--  - index
--  - other args ()
--  - buffername
--  - bufferlen
--  - buffername
--  - bufferlen
--  - ...
convPShuffler :: Pipeline -> Stmt -> Exp -> Pipeline
convPShuffler x (Shuffler name argnames bodys) (App n _ otherArgs) = x & shuffle ?~ appliedActualShuffs
  where
    namedArgs :: [Either (String, (GPUBuffer, Exp)) (String, Exp)]
    namedArgs = zipWith argsJ (tail argnames) argValues
      where
        argsJ name (Left x) = Left (name, x)
        argsJ name (Right x) = Right (name, x)

        argValues :: [Either (GPUBuffer, Exp) Exp]
        argValues =
          concat $
            map Right otherArgs :
            map (\bs@(GPUBuffer _ size, _) -> [Left bs, Right $ Int size]) (shuffledOutBuf x)

    -- List mapping variable names to GPUBuffers
    gpuBufferArgs :: [(String, (GPUBuffer, Exp))]
    gpuBufferArgs = lefts namedArgs

    lookupGPUBuff :: String -> (GPUBuffer, Exp)
    lookupGPUBuff n =
      fromMaybe
        (error $ "Could not find buffer " ++ n ++ " for call to Shuffler " ++ name ++ " (availible buffers " ++ show gpuBufferArgs ++ ")")
        $ lookup n gpuBufferArgs

    indexArgMapping = (Var (head argnames) False, indexVar)

    -- Substitutions to apply to the bodies of the shuffler
    nonGpuBufferArgs :: [(Exp, Exp)]
    nonGpuBufferArgs = indexArgMapping : map fstToVar (rights namedArgs)

    -- Shufflers must end in an array access.
    -- This function extracts the name of the array and maps it back to a GPUBuffer
    --
    -- Returns (     Target        , shuffleToApply)
    -- Returns ((buffer, oldSuffle), shuffleToApply)
    -- Type checker should ensure this works!!!
    expToShuff :: Exp -> ((GPUBuffer, Exp), Exp)
    expToShuff (ArrayGet (Var name False) movement) = (lookupGPUBuff name, movement)
    expToShuff _ = error "Suffler must output an arrayGet"

    -- Actual shuffle: mapping GPUBuffer to Expression that may contain a (Var "index" False) that should NOT be replaced.
    -- The (Var "index" False) in the current shuffle should be replaced by the expression
    actualShuffs :: [((GPUBuffer, Exp), Exp)]
    actualShuffs = map expToShuff bodys

    appliedActualShuffs :: [(GPUBuffer, Exp)]
    appliedActualShuffs = map applyShuff actualShuffs

    applyShuff :: ((GPUBuffer, Exp), Exp) -> (GPUBuffer, Exp)
    applyShuff ((buff, old), movement) = (buff, subst indexVar (substMult nonGpuBufferArgs movement) old)

-- A loop begins and ends with a stored array (for simplicity atm)
-- TODO: make more simple
convPLoop :: SCode Pipeline -> Exp -> SCode Pipeline
convPLoop y (Loop (Int 0) _ _) = y -- do nothing when 0 iterations
convPLoop y (Loop (Int n) itterName steps) | n > 0 = do
  x <- y
  let preLoopBufSufs = shuffledOutBuf x
  -- Old data is coppies into this buffer (taking shuffle into account)
  loopStartBuffers <- mapM (\(GPUBuffer _ s, _) -> freshGPUBuffer s) preLoopBufSufs
  -- The first step will read from loopStartBuffers and put them in firstOutputBuffers
  firstOutputBuffers <- mapM (\(GPUBuffer _ s, _) -> freshGPUBuffer s) preLoopBufSufs
  loopBody <-
    mapM
      ( \i ->
          foldl
            convP
            ( return $ -- Initial = simply read the coppied buffer
                Pipeline
                  { _shuffle = Nothing,
                    _curExp = copyBufExp loopStartBuffers firstOutputBuffers,
                    _doneExps = []
                  }
            )
            (map (subst (Var itterName False) (Int i)) steps)
      )
      (reverse [0 .. (n -1)])
  let loopExpFull =
        concatMap
          (\bdy -> copyBufExp (bdy ^. (curExp . outBuf)) loopStartBuffers : dataToList bdy)
          loopBody
  return $
    x
      & shuffle .~ Nothing
      & curExp .~ head loopExpFull -- first step of repeated loop
      & doneExps
        %~ ( \t ->
               tail loopExpFull -- repeated loop
                 ++ copyBufShufExp preLoopBufSufs loopStartBuffers : -- Copy to input buffer
               (x ^. curExp) : -- Do last action before loop
               t -- do all previous actions
           )
convPLoop y (Loop expr _ _) = error $ "Using loop with non-int value:" ++ show expr

assert :: Int -> Int -> a -> a
assert a b | a == b = id
assert a b = \_ -> error $ "Assertion Failed : " ++ show [a, b]

-- A map if the last was also a map can be combined

convPMapper :: Pipeline -> Stmt -> Exp -> SCode Pipeline
convPMapper x@Pipeline {_shuffle = Nothing} (Mapper _ argNames bodys) (App n _ args) =
  return $
    x
      & (curExp . expValue) .~ map (substMult $ zip ((`Var` False) <$> argNames) (indexVar : args ++ (x ^. (curExp . expValue)))) bodys
-- If the last step was a shuffle, we must be carefull
convPMapper x@Pipeline {_shuffle = Just s} (Mapper _ argNames bodys) (App n _ args) = do
  freshBuffers <- mapM (\(GPUBuffer _ s, _) -> freshGPUBuffer s) $ shuffledOutBuf x
  return $
    x
      & curExp
        .~ PipelineStep
          { _outBuf = freshBuffers, -- Out is new buffer
            _expValue =
              assert (length argNames) (length s + length args + 1) $ -- todo remove check
                map
                  ( substMult $
                      zip ((`Var` False) <$> argNames) $ indexVar : args ++ map (uncurry gpuBufferGet) s
                  )
                  bodys -- expression is simple array access with map applied to shuffled dat
          }
      & doneExps %~ ((x ^. curExp) :) -- prepend the old curExp
      & shuffle .~ Nothing

-- An empty shuffle simply selects the i-th element for every buffer
emptySuffle = map (,indexVar)

emptyData :: [GPUBuffer] -> Pipeline
emptyData buffers =
  Pipeline
    { _shuffle = Nothing, -- select the i-th element of every buffer
      _curExp =
        PipelineStep
          { _outBuf = buffers,
            _expValue = replicate (length buffers) indexVar -- default value is just the index (there is no inBuf to read from)
          },
      _doneExps = []
    }

-- There is a way to make this more efficeint by passing on the suffle and the expValue (is some cases)

copyBufExp :: [GPUBuffer] -> [GPUBuffer] -> PipelineStep
copyBufExp inBuf = copyBufShufExp (map (,indexVar) inBuf)

copyBufShufExp :: [(GPUBuffer, Exp)] -> [GPUBuffer] -> PipelineStep
copyBufShufExp inBufsAndShuffles target =
  PipelineStep
    { _outBuf = target,
      _expValue = map (uncurry gpuBufferGet) inBufsAndShuffles
    }

-- Convert a pipeline data into a list of Pipeline steps
-- Note this list must be reversed just before generating code
dataToList :: Pipeline -> [PipelineStep]
dataToList x@Pipeline {_shuffle = Nothing} = (x ^. curExp) : (x ^. doneExps) -- only if the shuffle is the simple shuffle
dataToList x@Pipeline {_shuffle = s, _curExp = p} = error $ "shuffle present " ++ show s ++ "-----" ++ show p

-- Get a list of used GPUBuffers:
collectBuffers :: [PipelineStep] -> [GPUBuffer]
collectBuffers p = Set.toList $ foldl addO Set.empty p
  where
    addO s PipelineStep {_outBuf = buffers} = foldr Set.insert s buffers

-- Convert a pipe into kernel specifications
-- We build a Pipeline and

arrayName num = "array" ++ show num

-- Builtin var that represents the index
indexVar = Var "index" True

-- Convert pipeline step
-- Allocated needed buffers
-- Call code
-- Read output buffers
convertPls :: (Exp -> SCode String) -> [Exp] -> SCode ()
convertPls mkKernelCode exps = do
  plSteps <- convertPipe exps
  mapM_ (addHostCode . AllocBuffer) $ collectBuffers plSteps
  foldlM convert [] plSteps -- fold keeps pervious output buffer
  mapM_ (addHostCode . ReadBuffer) (_outBuf $ last plSteps)
  where
    convert :: [GPUBuffer] -> PipelineStep -> SCode [GPUBuffer]
    convert inBuf x = do
      let argBufs = inBuf ++ (x ^. outBuf)
      fName <- addDeviceKernel mkKernelCode (_expValue x) argBufs (x ^. outBuf)
      addHostCode $ CallKernel fName argBufs (gpuBufferSize $ last argBufs)
      return (x ^. outBuf)

fstToVar (a, b) = (Var a False, b)
