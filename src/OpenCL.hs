module OpenCL
  ( OpenCLAction (..),
    GPUBuffer (..),
    Range (..),
    OpenCLRunner,
    mkOpenRunner,
    gpuBufferSize,
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

data GPUBuffer = GPUBuffer String Int deriving (Show, Eq, Ord)
gpuBufferSize (GPUBuffer _ s) = s

data Range = Range Int Int Int deriving (Show)

data OpenCLAction
  = MakeKernel String [GPUBuffer] Range
  | AllocBuffer GPUBuffer
  | ReadBuffer GPUBuffer
  deriving (Show)

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
  return $ OpenCLRunner (runAction context q program [] []) (Nothing)

showOpenRunner :: OpenCLRunner -> IO String
showOpenRunner (OpenCLRunner _ (Just ptr)) = ptr show
showOpenRunner (OpenCLRunner _ Nothing) = return "Nothing"

runList :: IO OpenCLRunner -> [OpenCLAction] -> IO OpenCLRunner
runList oclr [] = oclr
runList ioclr (a : r) = do
  (OpenCLRunner f _) <- ioclr
  putStrLn "--"
  print a
  runList (f a) r

runAction :: CLContext -> CLCommandQueue -> CLProgram -> [(GPUBuffer, CLMem)] -> [CLEvent] -> OpenCLAction -> IO OpenCLRunner
runAction c q p g waitFor (MakeKernel name args range) = do
  print waitFor
  kernel <- clCreateKernel p name
  mapM_ (\(i, n) -> clSetKernelArgSto kernel i $ getBuf n) (zip [0 ..] args)
  evt <- clEnqueueNDRangeKernel q kernel (rangeArr range) [] waitFor
  return $ OpenCLRunner (runAction c q p g [evt]) Nothing
  where
    getBuf n = fromMaybe (error $ "Could not find buffer " ++ show n) (lookup n g)
runAction c q p g waitFor (AllocBuffer gpub@(GPUBuffer _ size)) = do
  let elemSize = sizeOf (0 :: CInt)
      vecSize = elemSize * size
  print waitFor
  mem_in <- clCreateBuffer c [CL_MEM_READ_WRITE] (vecSize, nullPtr)
  return $ OpenCLRunner (runAction c q p ((gpub, mem_in) : g) waitFor) Nothing
runAction c q p g waitFor (ReadBuffer gpub@(GPUBuffer _ size)) = do
  putStrLn $ show waitFor
  let elemSize = sizeOf (0 :: CInt)
      vecSize = elemSize * size
  input <- mallocArray size :: IO (Ptr CInt)
  let (Just cbuf) = (lookup gpub g)
  putStrLn $ show cbuf
  putStrLn $ show vecSize
  evt <- clEnqueueReadBuffer q cbuf True 0 vecSize (castPtr input) waitFor
  return $
    OpenCLRunner (runAction c q p g [evt]) $
      Just $
        \f -> do
          l <- peekArray size input
          return $ f l
