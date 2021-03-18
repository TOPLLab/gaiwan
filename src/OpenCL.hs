module OpenCL
  ( OpenCLAction (..),
    CLGPUBuffer (..),
    Range (..),
    OpenCLRunner (), -- Constructor hidden
    mkOpenRunner,
    mkOpenRunnerInteger,
    run,
  )
where

import qualified Control.Exception as Ex (catch)
import Control.Monad
import Control.Parallel.OpenCL
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C
import System.Exit

-- | An openCL buffer with a certrain Id and a size
data CLGPUBuffer = CLGPUBuffer Int Int deriving (Show, Eq, Ord)

-- | A range
data Range = Range Int Int Int deriving (Show)

-- | OpenCL actions that can be performed
data OpenCLAction
  = MakeKernel String [CLGPUBuffer] Range
  | AllocBuffer CLGPUBuffer
  | FreeBuffer CLGPUBuffer
  | ReadBuffer CLGPUBuffer
  deriving (Show)

-- | Information to keep between GPU invocations
data RunCLData a = RunCLData
  { context :: CLContext,
    queue :: CLCommandQueue,
    program :: CLProgram,
    gpuBuffers :: [(CLGPUBuffer, CLMem)], -- buffer mapping
    waitlist :: [CLEvent], -- events to wait for
    convertor :: [CInt] -> a
  }

-- | OpenCLRunner executor lastValue
-- Takes an action and returns a new executor
data OpenCLRunner a = OpenCLRunner (OpenCLAction -> IO (OpenCLRunner a)) (Maybe a)

rangeArr (Range a 0 0) = [a]
rangeArr (Range a b 0) = [a, b]
rangeArr (Range a b c) = [a, b, c]

-- | Make a runner that can run OpenCLAction s to an [Interger]
-- The argument is the OpenCL host code
mkOpenRunnerInteger :: String -> IO (OpenCLRunner [Integer])
mkOpenRunnerInteger = mkOpenRunner $ map toInteger

-- | Make a runner that can run OpenCLAction s to a chosen type
-- The converor of CInt to a must be suplied and the host code
mkOpenRunner :: ([CInt] -> a) -> String -> IO (OpenCLRunner a)
mkOpenRunner convertor programSource = do
  -- Initialize OpenCL
  (platform : _) <- clGetPlatformIDs
  (dev : _) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [] [dev] print
  q <- clCreateCommandQueue context dev [CL_QUEUE_PROFILING_ENABLE]

  -- Initialize Kernel
  program <- clCreateProgramWithSource context programSource
  Ex.catch
    (clBuildProgram program [dev] "")
    ( \CL_BUILD_PROGRAM_FAILURE -> do
        log <- clGetProgramBuildLog program dev
        putStrLn "Compilation failed!"
        putStrLn programSource
        putStrLn "---"
        putStrLn log
        exitFailure -- TODO: get rid of this
    )
  let initialData =
        RunCLData
          { context = context,
            queue = q,
            program = program,
            gpuBuffers = [],
            waitlist = [],
            convertor = convertor
          }
  --putStrLn programSource

  return $ OpenCLRunner (runAction initialData) Nothing

-- | Run a series of actions in an OpenCLRunner and get the result
run :: OpenCLRunner a -> [OpenCLAction] -> IO [a]
run oclr list = do
  snd <$> foldM collectResults (oclr, []) list
  where
    -- Accumulator: (runner, collected return values)
    collectResults (OpenCLRunner f _, acc) action = do
      -- print action
      next@(OpenCLRunner _ v) <- f action
      case v of
        Just returnValue -> return (next, returnValue : acc) -- collect return
        Nothing -> return (next, acc)

-- | Lookup the "real" GPUBuffer based on the CLGPUBuffer
getGpuBuffer :: RunCLData a -> CLGPUBuffer -> CLMem
getGpuBuffer d buffer = fromMaybe (error "could not find buffer") $ lookup buffer (gpuBuffers d)

-- | Run a single action, meant to be run from
runAction :: RunCLData a -> OpenCLAction -> IO (OpenCLRunner a)
runAction d (MakeKernel name args range) = do
  kernel <- clCreateKernel (program d) name
  zipWithM_ (\i n -> clSetKernelArgSto kernel i $ getGpuBuffer d n) [0 ..] args
  evt <- clEnqueueNDRangeKernel (queue d) kernel (rangeArr range) [] (waitlist d)
  returnAction (d {waitlist = [evt]}) Nothing
runAction d (AllocBuffer gpub@(CLGPUBuffer _ size)) = do
  let elemSize = sizeOf (0 :: CInt)
      vecSize = elemSize * size
  mem_in <- clCreateBuffer (context d) [CL_MEM_READ_WRITE] (vecSize, nullPtr)
  returnAction (d {gpuBuffers = (gpub, mem_in) : gpuBuffers d}) Nothing
runAction d (FreeBuffer gpub) = do
  let cbuf = getGpuBuffer d gpub
  clReleaseMemObject cbuf
  returnAction (d {gpuBuffers = filter (\(b, _) -> b /= gpub) (gpuBuffers d)}) Nothing
runAction d (ReadBuffer gpub@(CLGPUBuffer _ size)) = do
  let elemSize = sizeOf (0 :: CInt)
      vecSize = elemSize * size
  input <- mallocArray size :: IO (Ptr CInt)
  let cbuf = getGpuBuffer d gpub
  evt <- clEnqueueReadBuffer (queue d) cbuf True 0 vecSize (castPtr input) (waitlist d)
  contents <- peekArray size input
  -- send the read contents to the convertor in d
  returnAction (d {waitlist = [evt]}) $ Just (convertor d contents)

-- | Update the runner and return value
returnAction :: RunCLData a -> Maybe a -> IO (OpenCLRunner a)
returnAction v r = return $ OpenCLRunner (runAction v) r
