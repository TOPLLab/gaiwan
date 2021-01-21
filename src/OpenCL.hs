module OpenCL
  ( OpenCLAction (..),
    CLGPUBuffer (..),
    Range (..),
    OpenCLRunner,
    mkOpenRunner,
    runList,
    showOpenRunner,
  )
where

import qualified Control.Exception as Ex (catch)
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

data RunCLData = RunCLData
  { context :: CLContext,
    queue :: CLCommandQueue,
    program :: CLProgram,
    gpuBuffers :: [(CLGPUBuffer, CLMem)], -- buffer mapping
    kernels :: [(String, CLKernel)],
    waitlist :: [CLEvent] -- events to wait for
  }

-- OpenCLRunner executor renderrer?
data OpenCLRunner = OpenCLRunner (OpenCLAction -> IO OpenCLRunner) (Maybe (([CInt] -> String) -> IO String))

rangeArr (Range a 0 0) = [a]
rangeArr (Range a b 0) = [a, b]
rangeArr (Range a b c) = [a, b, c]

mkOpenRunner programSource = do
  -- Initialize OpenCL
  (platform : _) <- clGetPlatformIDs
  (dev : _) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [] [dev] print
  q <- clCreateCommandQueue context dev [CL_QUEUE_PROFILING_ENABLE]
  putStrLn programSource

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
            waitlist = []
          }
  return $ OpenCLRunner (runAction initialData) Nothing

showOpenRunner :: OpenCLRunner -> IO String
showOpenRunner (OpenCLRunner _ (Just ptr)) = ptr show
showOpenRunner (OpenCLRunner _ Nothing) = return "Nothing"

runList :: IO OpenCLRunner -> [OpenCLAction] -> IO OpenCLRunner
runList oclr [] = oclr
runList ioclr (a : r) = do
  oclr <- ioclr
  let OpenCLRunner f _ = oclr
  putStrLn "--> Latest output>"
  s <- showOpenRunner oclr
  putStrLn s
  putStrLn "--<"
  print a
  putStrLn "--"
  print a
  runList (f a) r


getGpuBuffer :: RunCLData -> CLGPUBuffer -> CLMem
getGpuBuffer d buffer = fromMaybe (error "could not find buffer") $ lookup buffer (gpuBuffers d)

runAction :: RunCLData -> OpenCLAction -> IO OpenCLRunner
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
  print cbuf
  print vecSize
  evt <- clEnqueueReadBuffer (queue d) cbuf True 0 vecSize (castPtr input) (waitlist d)
  return $
    OpenCLRunner (runAction d {waitlist = [evt]}) $
      Just $ \shower -> do
        contents <- peekArray size input
        return $ shower contents
