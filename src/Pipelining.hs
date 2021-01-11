{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Pipelining (Pipeline, analyseArrays, convertPls) where

import Code
import Control.Lens
import Data.List
import qualified Data.Set as Set
import Language.Gaiwan
import Debug.Trace

data PipelineStep = PipelineStep
  { _inBuf :: [GPUBuffer],
    _outBuf :: [GPUBuffer],
    _expValue :: Exp
  }
  deriving (Show)

makeLenses ''PipelineStep

data Pipeline = Pipeline
  { _shuffle :: Exp,
    _lastMap :: Bool, -- if the previous step was a map
    _curExp :: PipelineStep, -- TODO: should be a LIST
    _doneExps :: [PipelineStep],
    _curSize :: Int -- todo: what is the point of this field?
  }
  deriving (Show)

makeLenses ''Pipeline

-- Substiute a for b in c
subst :: Exp -> Exp -> Exp -> Exp
subst a b c | a == c = b
subst (Var a _) b l@(Loop int varname exps) | a == varname = l
subst a@(Var _ _) b c = _subst c
  where -- cases below are the non-special cases
    _subst (Let string exp exp2) = undefined
    _subst (Plus x y) = Plus (recCall a) (recCall b)
    _subst (Minus x y) = Minus (recCall a) (recCall b)
    _subst (App name t args) = App name t (map recCall args)
    _subst (Modulo x y) = Modulo (recCall x) (recCall y)
    _subst (Times x y) = Times (recCall x) (recCall y)
    _subst (Div x y) = Div (recCall x) (recCall y)
    _subst v@Int {} = v
    _subst v@Var {} = v -- actual replacement done in 2nd clause
    _subst (Negate exp) = Negate (recCall exp)
    _subst (PipedExp exps) = PipedExp (map _subst exps)
    _subst (ArrayGet exp idx) = ArrayGet (recCall exp) (recCall idx)
    _subst (Loop cnt varname exps) = Loop cnt varname (map recCall exps)
    recCall = subst a b

-- Normalise the Exp (always use array0, array1, ...)
convertPipe :: [Exp] -> SCode [PipelineStep]
convertPipe (h : r) = reverse . dataToList <$> foldl convP (convH h) r
  where
    -- convert first item of the pipe
    convH :: Exp -> SCode Pipeline
    convH (App "generateSeq" True [Int n]) = emptyData <$> freshGPUBuffer n
    convH _ = error "Unknown start sequence"

    -- converts subsequent steps
    -- (we need the SCode State monad to translate names into calls to the right kind of steps)
    convP :: SCode Pipeline -> Exp -> SCode Pipeline
    convP x a@Loop {} = convPLoop x a
    convP x a@(App n _ _) = do
      t <- lookupDef n
      y <- x
      case t of
        Just Shuffler {} -> return $ convPShuffler y a
        Just Mapper {} -> convPMapper y a
        _ -> error $ "Unknown name " ++ show n
    -- A shuffle is stored in a variable for later reference, when we actually need it.
    -- A shuffler has at least 2 args (size and current index) and some other optional arguments
    convPShuffler :: Pipeline -> Exp -> Pipeline
    convPShuffler x (App n _ otherArgs) =
      x
        & lastMap .~ False
        --   Apply the suffle operation with arguments
        --   1) current size, 2) the current index = result of the previous shuffle 3) optiona extra args
        & shuffle
          .~ App n False (Int (x ^. curSize) : ((x ^. shuffle) : otherArgs))
    -- A loop begins and ends with a stored array (for simplicity atm)
    convPLoop :: SCode Pipeline -> Exp -> SCode Pipeline
    convPLoop y (Loop n itterName steps) = do
      x <- y
      bufferToCopy <- mapM (\(GPUBuffer _ s) -> freshGPUBuffer s) (x ^. (curExp . outBuf))
      -- fresh buffers to work with
      loopBuffer <- mapM freshGPUBuffer [x ^. curSize]
      loopBody <-
        foldl
          convP
          ( return $
              Pipeline
                { _shuffle = indexVar,
                  _lastMap = True,
                  _curExp -- copyExp bufferToCopy loopBuffer
                  =
                    PipelineStep
                      { _inBuf = bufferToCopy, -- A copy of the out of the previous step (copy done below)
                        _outBuf = loopBuffer, -- Out is new buffer
                        _expValue = gpuBufferGet (head bufferToCopy) indexVar -- expression is simple array access with map applied to shuffled data (moet eigenlijk een map zijn ofzo)
                      },
                  _doneExps = [],
                  _curSize = x ^. curSize
                }
          )
          steps
      let loopIterExp = copyBufExp (loopBody ^. (curExp . outBuf)) bufferToCopy : dataToList loopBody
      let loopExpFull = concatMap (\it -> map (over expValue $ subst (Var itterName False) (Int it)) loopIterExp) [0..(n-1)]
      return $
        x
          & shuffle .~ indexVar
          & lastMap .~ False
          & curExp .~ head loopExpFull
          & doneExps
            %~ ( \t ->
                   tail loopExpFull
                     ++ copyBufExp (x ^. (curExp . outBuf)) bufferToCopy : -- repeated loop
                     -- Copy to input buffer
                   (x ^. curExp) : -- Do last action before loop
                   t -- do all previous actions
               )
          & lastMap .~ True
    -- A map if the last was also a map can be combined
    convPMapper :: Pipeline -> Exp -> SCode Pipeline
    convPMapper x@Pipeline {_lastMap = True} (App n _ args) =
      return $
        x
          & lastMap .~ True
          & (curExp . expValue) .~ App n False (args ++ [x ^. (curExp . expValue)])
    convPMapper x@Pipeline {_lastMap = False} (App n _ args) = do
      freshBuffer <- freshGPUBuffer (x ^. curSize)
      return $
        x
          & curExp
            .~ PipelineStep
              { _inBuf = x ^. (curExp . outBuf), --in of next is out of prev
                _outBuf = [freshBuffer], -- Out is new buffer
                _expValue = App n False $ gpuBufferGet (head $ x ^. (curExp . outBuf)) (x ^. shuffle) : args -- expression is simple array access with map applied to shuffled data
              }
          & doneExps %~ ((x ^. curExp) :) -- prepend the old curExp
          & shuffle .~ indexVar
          & lastMap .~ True

emptyData buffer@(GPUBuffer _ size) =
  Pipeline
    { _shuffle = indexVar,
      _lastMap = True,
      _curExp =
        PipelineStep
          { _inBuf = [],
            _outBuf = [buffer],
            _expValue = indexVar -- default value is just the index (there is no inBuf to read from)
          },
      _doneExps = [],
      _curSize = size
    }

-- There is a way to make this more efficeint by passing on the suffle and the expValue (is some cases)
copyBufExp inBuf outBuf =
  PipelineStep
    { _inBuf = inBuf, -- A copy of the out of the previous step (copy done below)
      _outBuf = outBuf, -- Same sized output buffers
      _expValue = gpuBufferGet (head inBuf) indexVar -- expression is simple array access with map applied to shuffled data
    }

-- Convert a pipeline data into a list of Pipeline steps
-- Note this list must be reversed just before generating code
dataToList :: Pipeline -> [PipelineStep]
dataToList x@Pipeline {_shuffle = (Var "index" True)} = (x ^. curExp) : (x ^. doneExps)
dataToList _ = error "shuffle present"

analyseArrays :: [PipelineStep] -> [GPUBuffer]
analyseArrays p = Set.toList $ foldl addI Set.empty p
  where
    addI s PipelineStep {_inBuf = i, _outBuf = o} = foldr Set.insert s (i ++ o)

-- Convert a pipe into kernel specifications
-- We build a Pipeline and

arrayName num = "array" ++ show num

indexVar = Var "index" True

convertPls :: (Exp -> SCode String) -> [Exp] -> SCode ()
convertPls mkKernelCode exps = do
  plSteps <- convertPipe exps
  mapM_ (addHostCode . AllocBuffer) $ analyseArrays plSteps
  mapM_ convert plSteps
  mapM_ (addHostCode . ReadBuffer) (_outBuf $ last plSteps)
  where
    convert x = do
      let argBufs = (x ^. inBuf) ++ (x ^. outBuf)
      fName <- addDeviceKernel mkKernelCode (_expValue x) argBufs (x ^. outBuf)
      addHostCode $ CallKernel fName argBufs (gpuBufferSize $ last argBufs)
      return ()
