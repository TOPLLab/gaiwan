{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CodeGen.Pipelining (Pipeline, collectBuffers, convertPls) where

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

type IndexedOffset = (Int, Int)

-- | Set value Buffer[_ * i + _] = _
--                               buff  [  a*i+ b ]= e
data IndexedSet = IndexedSet GPUBuffer [(IndexedOffset, Exp)]
  deriving (Show)

simpleSet buf exp = IndexedSet buf [((1, 0), exp)]

newtype PipelineStep = PipelineStep [IndexedSet]
  deriving (Show)


makeLenses ''PipelineStep

data Pipeline = Pipeline
  { _shuffle :: Maybe [(GPUBuffer, Exp, Int)], -- Shuffle to apply to the _expValue of the _curExp → Sets the index to write to in the specified buffer?
    _curExp :: PipelineStep, -- TODO: should be a LIST
    _doneExps :: [PipelineStep]
    -- _curSize :: Int -- todo: what is the point of this field?
  }
  deriving (Show)

makeLenses ''Pipeline

-- Normalize the Exp (always use array0, array1, ...)
convertPipe :: Bool -> [Exp] -> SCode a [PipelineStep]
convertPipe collapse (h : r) = reverse . dataToList <$> foldl (convP collapse) (convH h) r
  where
    -- convert first item of the pipe
    convH :: Exp -> SCode a Pipeline
    convH (App "generateSeq" True [Int count, Int n]) = do
      buffers <- replicateM count $ freshGPUBuffer n
      return $ emptyData buffers
    convH _ = error "Unknown start sequence"

-- | Convert a step in a pipeline (Exp) and add it to the pipeline
-- If the first argument is True, subseqent maps and shuffles are merged
convP :: Bool -> SCode a Pipeline -> Exp -> SCode a Pipeline
convP collapse x = convP2 (if collapse then x else x >>= reifyShuffle)
  where
    convP2 :: SCode a Pipeline -> Exp -> SCode a Pipeline
    convP2 x a@Loop {} = convPLoop collapse x a
    -- Builtins
    convP2 x a@(App "split" True [Int numBuf, Int offset]) | numBuf > 0 && offset > 0 = convPSplitter numBuf offset <$> x
    convP2 x a@(App "join" True [Int numBuf, Int offset]) | numBuf > 0 && offset > 0 = x >>= convPJoin numBuf offset
    convP2 x a@(App "join" True arg) = error $ "Join: bad arguments " ++ show arg
    convP2 x a@(App name True _) = error $ "Unknown builtin function name " ++ name
    -- Other calls
    convP2 x a@(App n False _) = do
      t <- lookupDef n
      y <- x
      case t of
        Just f@Shuffler {} -> return $ convPShuffler y f a
        Just f@Mapper {} -> convPMapper y f a
        _ -> error $ "Unknown name " ++ show n

-- | Transform the top PipeLine expression into a buffer read with a additional id shuffle
-- This prevents optimisations
reifyShuffle :: Pipeline -> SCode a Pipeline
reifyShuffle x = do
  newBufs <- mapM (\(_, _, s) -> freshGPUBuffer s) bufs
  return $
    x
      & shuffle ?~ emptySuffle newBufs
      & curExp .~ copyBufShufExp bufs newBufs
      & doneExps %~ ((x ^. curExp) :) -- prepend the old curExp
  where
    bufs = shuffledOutBuf x

-- | Execute the @join(numBuf, offet) action
convPJoin :: Int -> Int -> Pipeline -> SCode a Pipeline
convPJoin numBuf offset x = do
  -- Allocate one new buffer sized as the sum of the incoming buffers
  freshBuffers <-
    mapM
      (\buffers -> freshGPUBuffer (sum [s | (_, _, s) <- buffers]))
      groupedBuffers
  return $
    x
      & curExp
      .~ PipelineStep
        ( zipWith
            (\outBuffer group -> simpleSet outBuffer $ convToIf $ boehoe group)
            freshBuffers
            groupedBuffers
        )
      & doneExps %~ ((x ^. curExp) :) -- prepend the old curExp
      & shuffle
      .~ Nothing
  where
    groupedBuffers = groupByCnt $ shuffledOutBuf x
    -- Chop up in put array in bits of size numBuf
    -- Output is [ ListOfLenNumBuf ]
    groupByCnt :: [a] -> [[a]]
    groupByCnt [] = []
    groupByCnt l | length l >= numBuf = let (h, rest) = splitAt numBuf l in h : groupByCnt rest
    groupByCnt l = error $ "could not join list that is not a multiple of the arg, left: " ++ show (length l)

    -- Number of in the element buffers before we start reading buffer 1 again
    chunkSize = offset * numBuf

    -- TODO: rename
    boehoe :: [(GPUBuffer, Exp, Int)] -> [(Int, Exp)]
    boehoe = zipWith indexPos [0 ..]
    -- Creates a bufferGet that access the target buffer at the right position
    indexPos :: Int -> (GPUBuffer, Exp, Int) -> (Int, Exp)
    indexPos bufferIndex (buffer, shuff, len) =
      -- TODO: use len?
      ( bufferIndex,
        GPUBufferGet buffer $
          Plus
            (Times (Div shuff (Int chunkSize)) (Int offset))
            (Modulo shuff (Int offset))
      )

    -- A pair of bufferindex, and acess expression to a nested if
    convToIf :: [(Int, Exp)] -> Exp
    convToIf [(_, e)] = e -- last element
    convToIf ((bufferIndex, e) : rest) =
      If
        ( IsEq
            (Modulo (Div indexVar (Int offset)) (Int numBuf))
            (Int bufferIndex)
        )
        e
        (convToIf rest)

-- | Execute the @split(numBuf, offet) action
-- The suffle split is executed as a shuffle on the current buffer
-- The split is executed on each of the inputbuffers
convPSplitter :: Int -> Int -> Pipeline -> Pipeline
convPSplitter numBuf offset x =
  x
    & shuffle
      ?~ concatMap
        ( \(buf, shuff, len) ->
            map
              ( \bufferNum ->
                  ( buf, -- src buf
                    simpleSubst indexVar (indexPos bufferNum) shuff,
                    offset * (len `div` chunkSize) -- TODO: correct what to do with missing data???
                  )
              )
              [0 .. (numBuf -1)]
        )
        (shuffledOutBuf x)
  where
    --  Number of in the element buffers before we start reading buffer 1 again
    chunkSize = offset * numBuf

    --  transform the index to an expression accessing the right element
    -- \i -> (( i `div` offset) * (offset * numBuf) + (  i  `mod`  offset))
    --       \_________ Chunk index ______________/   \_offset in chuhk_/
    indexPos :: Int -> Exp
    indexPos bufferNum =
      Plus
        (Times (Div indexVar offsetE) (Int chunkSize))
        ( Plus
            (Int $ bufferNum * offset)
            (Modulo indexVar offsetE)
        )
    offsetE = Int offset

-- | Execute a shuffler
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
    -- Read the arguments into an Either GPUBufferArg RegularArg
    namedArgs :: [Either (String, (GPUBuffer, Exp, Int)) (String, Exp)]
    namedArgs = zipWith argsJ (tail argnames) argValues
      where
        argsJ name (Left x) = Left (name, x)
        argsJ name (Right x) = Right (name, x)

        argValues :: [Either (GPUBuffer, Exp, Int) Exp]
        argValues =
          concat $
            map Right otherArgs :
            map (\bs@(GPUBuffer _ _, _, size) -> [Left bs, Right $ Int size]) (shuffledOutBuf x)

    -- List mapping variable names to GPUBuffers
    gpuBufferArgs :: [(String, (GPUBuffer, Exp, Int))]
    gpuBufferArgs = lefts namedArgs

    lookupGPUBuff :: String -> (GPUBuffer, Exp, Int)
    lookupGPUBuff n =
      fromMaybe
        (error $ "Could not find buffer " ++ n ++ " for call to Shuffler " ++ name ++ " (availible buffers " ++ show gpuBufferArgs ++ ")") -- error can be caught with typechecking
        $ lookup n gpuBufferArgs

    indexArgMapping = (Var (head argnames) False, indexVar)

    -- Substitutions to apply to the bodies of the shuffler
    nonGpuBufferArgs :: [(Exp, Exp)]
    nonGpuBufferArgs = indexArgMapping : map fstToVar (rights namedArgs)

    -- Shufflers must end in an array access.
    -- This function extracts the name of the array and maps it back to a GPUBuffer
    --
    -- Returns (          Target         , shuffleToApply)
    -- Returns ((buffer, oldSuffle, size), shuffleToApply)
    -- Type checker should ensure this works!!!
    expToShuff :: Exp -> ((GPUBuffer, Exp, Int), Exp)
    expToShuff (ArrayGet (Var name False) movement) = (lookupGPUBuff name, movement)
    expToShuff _ = error "Suffler must output an arrayGet" -- can be caught by typechecking

    -- Get the shuffles specified in the body and apply
    -- returns ((targetBuffer, oldSuffle, size), newShuffle to apply)
    --
    -- Actual shuffle: mapping GPUBuffer to Expression that may contain a (Var "index" False) that should NOT be replaced.
    -- The (Var "index" False) in the current shuffle should be replaced by the expression
    extractedShuffs :: [((GPUBuffer, Exp, Int), Exp)]
    extractedShuffs = map expToShuff bodys

    -- Execute a shuffle
    applyShuff :: ((GPUBuffer, Exp, Int), Exp) -> (GPUBuffer, Exp, Int)
    applyShuff ((buff, old, size), movement) =
      ( buff,
        simpleSubst indexVar (simpleSubstMult nonGpuBufferArgs movement) old,
        size
      )

    -- The result
    appliedActualShuffs :: [(GPUBuffer, Exp, Int)]
    appliedActualShuffs = map applyShuff extractedShuffs

simpleExp :: PipelineStep -> Bool
simpleExp (PipelineStep l) = all (\(IndexedSet _ x) -> is x) l
  where is :: [(IndexedOffset, Exp)] -> Bool
        is [((1,0), _)] = True
        is _ = False
        -- TODO rename

simpleExpExtract :: PipelineStep -> [Exp]
simpleExpExtract (PipelineStep l) = map (\(IndexedSet _ [((1,0), x)]) -> x) l


-- | Execute a mapper
-- If a mapper follows a mapper they are combined
convPMapper :: Pipeline -> Stmt -> Exp -> SCode a Pipeline
convPMapper x@Pipeline {_shuffle = Nothing} (Mapper _ argNames bodys) (App n _ args) | simpleExp (x ^. curExp) =
  return $
    x
      & curExp
        .~ map
          ( simpleSubstMult $
              zip ((`Var` False) <$> argNames) $
                indexVar : args ++ simpleExpExtract (x ^. curExp)
          )
          bodys
-- If the last step was a shuffle, we must be carefull
convPMapper x@Pipeline {_shuffle = Just s} (Mapper _ argNames bodys) (App n _ args) = do
  freshBuffers <- mapM (\(GPUBuffer _ _, _, size) -> freshGPUBuffer size) $ shuffledOutBuf x
  return $
    x
      & curExp
      .~ PipelineStep
        ( zipWith simpleSet freshBuffers $
            map
              ( simpleSubstMult $
                  zip ((`Var` False) <$> argNames) $
                    indexVar : args ++ map (\(b, i, _) -> GPUBufferGet b i) s
              )
              bodys
        )
      & doneExps %~ ((x ^. curExp) :) -- prepend the old curExp
      & shuffle .~ Nothing

-- An empty shuffle simply selects the i-th element for every buffer
emptySuffle = map (\b@(GPUBuffer _ s) -> (b, indexVar, s))

-- A loop begins and ends with a stored array (for simplicity atm)
-- TODO: make more simple
convPLoop :: Bool -> SCode a Pipeline -> Exp -> SCode a Pipeline
convPLoop _ y (Loop (Int 0) _ _) = y -- do nothing when 0 iterations
convPLoop collapse y (Loop (Int n) itterName steps) | n > 0 = do
  foldl (convP collapse) y flattened
  where
    flattened = concatMap (\i -> map (simpleSubst (Var itterName False) (Int i)) steps) [0 .. n -1]
convPLoop _ y (Loop expr _ _) = error $ "Using loop with non-int value:" ++ show expr

-- | Simple startdata assigned to specific buffers
-- The data is just 1,2,3,...
emptyData :: [GPUBuffer] -> Pipeline
emptyData buffers =
  Pipeline
    { _shuffle = Nothing, -- select the i-th element of every buffer
      _curExp =
        PipelineStep
          ( zipWith simpleSet buffers $
              replicate (length buffers) indexVar -- default value is just the index (there is no inBuf to read from)
          ),
      _doneExps = []
    }

-- | Make an expression to copy the contents of the first argumetn to the second argument
copyBufExp :: [GPUBuffer] -> [GPUBuffer] -> PipelineStep
copyBufExp inBuf = copyBufShufExp $ map (\b@(GPUBuffer _ s) -> (b, indexVar, s)) inBuf

-- | Make an expression to copy the contents of the first argumetn to the second argument applying a shuffele
copyBufShufExp :: [(GPUBuffer, Exp, Int)] -> [GPUBuffer] -> PipelineStep
copyBufShufExp inBufsAndShuffles target =
  PipelineStep
    ( zipWith simpleSet target $
        map (\(buf, shuf, _) -> GPUBufferGet buf shuf) inBufsAndShuffles
    )

-- | Convert a pipeline data into a list of Pipeline steps
-- Note this list must be reversed just before generating code
dataToList :: Pipeline -> [PipelineStep]
dataToList x@Pipeline {_shuffle = Nothing} = (x ^. curExp) : (x ^. doneExps) -- only if the shuffle is the simple shuffle
dataToList x@Pipeline {_shuffle = s, _curExp = p} = error $ "shuffle present " ++ show s ++ "-----" ++ show p

-- | Get a list of used GPUBuffers
collectBuffers :: [PipelineStep] -> [GPUBuffer]
collectBuffers p = Set.toList $ foldl addO Set.empty p
  where
    addO :: Set.Set GPUBuffer -> PipelineStep -> Set.Set GPUBuffer
    addO s (PipelineStep indexedSet) = foldr (\(IndexedSet b _) -> Set.insert b) s indexedSet

-- | Builtin var that represents the index
indexVar = Var "index" True

-- | Extract the shuffle from the current pipeline
shuffledOutBuf :: Pipeline -> [(GPUBuffer, Exp, Int)]
shuffledOutBuf x = fromMaybe (emptySuffle (x ^. (curExp . outBuf))) $ x ^. shuffle

-- | Convert pipeline to a list of steps
--
-- Allocated needed buffers
-- Call code
-- Read output buffers
convertPls ::
  Monoid a =>
  Bool ->
  (Exp -> SCode a b) ->
  (KernelName -> [GPUBuffer] -> [GPUBuffer] -> [b] -> a) ->
  [Exp] ->
  SCode a ()
convertPls collapse mkKernelCode kernelTemplate exps = do
  plSteps <- convertPipe collapse exps
  foldlM convert [] plSteps -- fold keeps pervious output buffer
  mapM_ (addHostCode . ReadBuffer) (reverse $ _outBuf $ last plSteps)
  where
    convert inBuf x = do
      let argBufs = inBuf ++ (x ^. outBuf)
      fName <- addDeviceKernel mkKernelCode kernelTemplate (_expValue x) argBufs (x ^. outBuf)
      addHostCode $ CallKernel fName argBufs (x ^. outBuf) (gpuBufferSize $ last argBufs)
      return (x ^. outBuf)

fstToVar (a, b) = (Var a False, b)
