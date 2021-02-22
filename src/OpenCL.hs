module OpenCL
  ( OpenCLAction (..),
    CLGPUBuffer (..),
    Range (..),
    OpenCLRunner,
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

data CLGPUBuffer = CLGPUBuffer Int Int deriving (Show, Eq, Ord)

data Range = Range Int Int Int deriving (Show)

data OpenCLAction
  = MakeKernel String [CLGPUBuffer] Range
  | AllocBuffer CLGPUBuffer
  | ReadBuffer CLGPUBuffer
  deriving (Show)

data RunCLData a = RunCLData
  { context :: CLContext,
    queue :: CLCommandQueue,
    program :: CLProgram,
    gpuBuffers :: [(CLGPUBuffer, CLMem)], -- buffer mapping
    kernels :: [(String, CLKernel)],
    waitlist :: [CLEvent], -- events to wait for
    convertor :: [CInt] -> a
  }

-- OpenCLRunner executor lastValue
data OpenCLRunner a = OpenCLRunner (OpenCLAction -> IO (OpenCLRunner a)) (Maybe a)

rangeArr (Range a 0 0) = [a]
rangeArr (Range a b 0) = [a, b]
rangeArr (Range a b c) = [a, b, c]

mkOpenRunnerInteger :: String -> IO (OpenCLRunner [Integer])
mkOpenRunnerInteger = mkOpenRunner $ map toInteger

mkOpenRunner :: ([CInt] -> a) -> String -> IO (OpenCLRunner a)
mkOpenRunner convertor programSource = do
  -- Initialize OpenCL
  (platform : _) <- clGetPlatformIDs
  (dev : _) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [] [dev] print
  q <- clCreateCommandQueue context dev [CL_QUEUE_PROFILING_ENABLE]
  -- putStrLn programSource

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
            kernels = [],
            waitlist = [],
            convertor = convertor
          }
  --putStrLn programSource

  return $ OpenCLRunner (runAction initialData) Nothing

-- Run with a convertor
run :: OpenCLRunner a -> [OpenCLAction] -> IO [a]
run oclr list = do
  snd <$> foldM runAction (oclr, []) list
  where
    runAction (OpenCLRunner f _, acc) action = do
      -- print action
      next@(OpenCLRunner _ v) <- f action
      case v of
        Just sf -> return (next, sf : acc)
        Nothing -> return (next, acc)

getGpuBuffer :: RunCLData a -> CLGPUBuffer -> CLMem
getGpuBuffer d buffer = fromMaybe (error "could not find buffer") $ lookup buffer (gpuBuffers d)

runAction :: RunCLData a -> OpenCLAction -> IO (OpenCLRunner a)
runAction d (MakeKernel name args range) = do
  kernel <- clCreateKernel (program d) name
  mapM_ (\(i, n) -> clSetKernelArgSto kernel i $ getGpuBuffer d n) (zip [0 ..] args)
  evt <- clEnqueueNDRangeKernel (queue d) kernel (rangeArr range) [] (waitlist d)
  return $ OpenCLRunner (runAction d {waitlist = [evt]}) Nothing
runAction d (AllocBuffer gpub@(CLGPUBuffer _ size)) = do
  let elemSize = sizeOf (0 :: CInt)
      vecSize = elemSize * size
  mem_in <- clCreateBuffer (context d) [CL_MEM_READ_WRITE] (vecSize, nullPtr)
  return $ OpenCLRunner (runAction d {gpuBuffers = (gpub, mem_in) : gpuBuffers d}) Nothing
runAction d (ReadBuffer gpub@(CLGPUBuffer _ size)) = do
  let elemSize = sizeOf (0 :: CInt)
      vecSize = elemSize * size
  input <- mallocArray size :: IO (Ptr CInt)
  let cbuf = getGpuBuffer d gpub
  evt <- clEnqueueReadBuffer (queue d) cbuf True 0 vecSize (castPtr input) (waitlist d)
  contents <- peekArray size input
  return $
    OpenCLRunner (runAction d {waitlist = [evt]}) $
      Just (convertor d contents)
