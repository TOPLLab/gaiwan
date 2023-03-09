module OpenCL
  ( OpenCLAction (..),
    CLGPUBuffer (..),
    Range (..),
    OpenCLRunner (), -- Constructor hidden
    mkOpenRunner,
    convertPlan,
    run,
  )
where

import Code.Definitions (KernelName (KernelName), varFiller)
import qualified Code.Definitions as Defs
import qualified Control.Exception as Ex (catch)
import Control.Monad
import Control.Monad.State
import Control.Parallel.OpenCL
import Data.Bits
import Data.ByteString (useAsCStringLen)
import Data.ByteString as BS hiding (concat, filter, intercalate, map, putStrLn)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.List
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Word
import Debug.Trace
import Foreign
import Foreign.C
import Language.GaiwanDefs
import System.Exit
import System.FilePath

-- | An openCL buffer with a certrain Id and a size
data CLGPUBuffer = CLGPUBuffer Int Int deriving (Show, Eq)

-- | A range
data Range = Range Int Int Int deriving (Show, Eq)

-- | OpenCL actions that can be performed
data OpenCLAction
  = MakeKernel String [CLGPUBuffer] Range
  | AllocBuffer CLGPUBuffer
  | FreeBuffer CLGPUBuffer
  | Release
  | ReadBuffer (Ptr CInt) CLGPUBuffer
  | ExtractBuffer CLGPUBuffer
  deriving (Show, Eq)

-- | Information to keep between GPU invocations
data RunCLData a = RunCLData
  { context :: CLContext,
    queue :: CLCommandQueue,
    program :: CLProgram,
    gpuBuffers :: [(CLGPUBuffer, CLMem)], -- buffer mapping
    waitlist :: [CLEvent], -- events to wait for
    convertor :: Int -> Ptr CInt -> IO a
  }

-- | OpenCLRunner executor lastValue
-- Takes an action and returns a new executor
data OpenCLRunner a = OpenCLRunner (OpenCLAction -> IO (OpenCLRunner a)) (Maybe a)

rangeArr (Range a 0 0) = [a]
rangeArr (Range a b 0) = [a, b]
rangeArr (Range a b c) = [a, b, c]

convertPlan :: [Defs.GPUAction] -> IO (Either String ([OpenCLAction], [(String, Int)]))
convertPlan p = do
  let readMap = analyzeReads p
  returnedData <- mapM findRealData $ M.keys readMap
  let returnedMap = M.fromList returnedData
  let varFillFunEither =
        varFiller $
          M.elems
            ( M.intersectionWith
                (\(Defs.ReservedBuffer n gb) (len, _) -> (gb, (GaiwanInt, len)))
                readMap
                returnedMap
            )
  return $ case varFillFunEither of
    (Left s) -> Left s
    (Right varFillFun) -> do
      case runStateT (mapM (convertS returnedData varFillFun) p) M.empty of
        (Left s) -> Left s
        (Right (actions, assignedBuffers)) ->
          Right
            ( concat $ actions ++ [freeUsedBuffers assignedBuffers] ++ [[Release]],
              map defineLENDefines $ M.toList assignedBuffers
            )

freeUsedBuffers :: M.Map Defs.ReservedBuffer CLGPUBuffer -> [OpenCLAction]
freeUsedBuffers m = map FreeBuffer $ nub $ M.elems m

defineLENDefines :: (Defs.ReservedBuffer, CLGPUBuffer) -> (String, Int)
defineLENDefines
  ( Defs.ReservedBuffer gbn (GaiwanBuf (GaiwanBufSize n i j) gs),
    CLGPUBuffer _ size
    ) =
    ("LEN_" ++ show n ++ "_" ++ show i ++ "_" ++ show j, size)

findRealData :: String -> IO (String, (Int, Ptr CInt))
findRealData f = do
  dat <- BS.readFile $ "demo" </> "input" </> (f ++ ".int.gw")
  dd <- unsafeUseAsCStringLen dat $ \(toTmpPtr, len) -> do
    let newElemSize = sizeOf (0 :: CInt)
    let oldElemSize = sizeOf (0 :: CChar)
    arr <- mallocArray len :: IO (Ptr CChar)
    copyArray arr toTmpPtr len
    case divMod (len * oldElemSize) newElemSize of
      (intLen, 0) -> return (intLen, castPtr arr)
      _ -> fail $ "Could not convert " ++ show len ++ " chars of size " ++ show oldElemSize ++ " to ints of size " ++ show newElemSize
  return (f, dd)

analyzeReads :: [Defs.GPUAction] -> M.Map String Defs.ReservedBuffer
analyzeReads [] = M.empty
analyzeReads ((Defs.ReadBuffer s rb) : gas) = M.insert s rb $ analyzeReads gas
analyzeReads (_ : gas) = analyzeReads gas

convertS :: [(String, (Int, Ptr CInt))] -> (GaiwanBuf Int -> Either String (GShape Void, Int)) -> Defs.GPUAction -> StateT (M.Map Defs.ReservedBuffer CLGPUBuffer) (Either String) [OpenCLAction]
convertS _ _ (Defs.CallKernel (KernelName n) rbs [outBuf]) = do
  bufs <- mapM findBuf rbs
  bufo <- findBuf outBuf
  return [MakeKernel n (bufs ++ [bufo]) (case bufo of (CLGPUBuffer _ s) -> Range s 0 0)]
convertS _ _ (Defs.CallKernel kn rbs _) = fail "More than one outputBuf for CallKernel"
convertS _ _ (Defs.CallReducerKernel (KernelName n) rbs outBuf) = do
  bufs <- mapM findBuf rbs
  bufo <- findBuf outBuf
  return [MakeKernel n (bufs ++ [bufo]) (Range 1 0 0)] -- only one thread :(
convertS lt vt (Defs.ReadBuffer str rb@(Defs.ReservedBuffer gbn gb)) = do
  m <- get
  case M.lookup rb m of
    Just clb -> return [] -- nothing to do we already read the file to the buffer
    Nothing -> do
      (shape, size) <- lift $ vt gb
      let clbuf = CLGPUBuffer (Defs.reservedBufferId rb) size
      put $ M.insert rb clbuf m
      return [ReadBuffer (snd $ fromJust $ lookup str lt) clbuf]
convertS _ vt (Defs.AllocBuffer rb@(Defs.ReservedBuffer gbn gb)) = do
  m <- get
  case M.lookup rb m of
    Just clb -> return [] -- nothing to do we already read the file to the buffer
    Nothing -> do
      (shape, size) <- lift $ vt gb
      let clbuf = CLGPUBuffer (Defs.reservedBufferId rb) size
      put $ M.insert rb clbuf m
      return [AllocBuffer clbuf]
convertS _ vt (Defs.OutputBuffer rbs) = do
  m <- get
  case mapM (`M.lookup` m) rbs of
    Just clb -> return $ map ExtractBuffer clb
    Nothing -> fail "cannot outpurt buffers we did not allocate"
convertS _ vt (Defs.Infoz str) = return []

findBuf :: Defs.ReservedBuffer -> StateT (M.Map Defs.ReservedBuffer CLGPUBuffer) (Either String) CLGPUBuffer
findBuf k = do
  m <- get
  let lookupRestult = M.lookup k m
  case lookupRestult of
    Nothing -> fail $ "could not find " ++ show (k, m)
    (Just any) -> return any

mkOpenRunner :: (Int -> Ptr CInt -> IO a) -> String -> [(String, Int)] -> IO (OpenCLRunner a)
mkOpenRunner convertor programSource extraDefines = do
  -- Initialize OpenCL
  putStrLn "GOOOOOOO"
  (platform : _) <- clGetPlatformIDs
  print platform
  (dev : _) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [] [dev] print
  q <- clCreateCommandQueue context dev [CL_QUEUE_PROFILING_ENABLE]
  -- Initialize Kernel
  program <- clCreateProgramWithSource context programSource
  Ex.catch
    (clBuildProgram program [dev] (unwords $ map (\(name, v) -> "-D " ++ show name ++ "=" ++ show v) extraDefines))
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
  -- putStrLn programSource

  return $ OpenCLRunner (runAction initialData) Nothing

-- | Run a series of actions in an OpenCLRunner and get the result
run :: OpenCLRunner a -> [OpenCLAction] -> IO [a]
run oclr list =
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
-- runAction _ a | trace (show a) False = undefined
runAction d (MakeKernel name args range) = do
  kernel <- clCreateKernel (program d) name
  zipWithM_ (\i n -> clSetKernelArgSto kernel i $ getGpuBuffer d n) [0 ..] args
  evt <- clEnqueueNDRangeKernel (queue d) kernel (rangeArr range) [] (waitlist d)
  clReleaseKernel kernel
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
-- renew definition to read buffer
runAction d Release = do
  clReleaseContext (context d)
  clReleaseCommandQueue (queue d)
  clReleaseProgram (program d)
  returnAction (d {gpuBuffers = []}) Nothing
-- renew definition to read buffer
runAction d (ExtractBuffer gpub@(CLGPUBuffer _ size)) = do
  let elemSize = sizeOf (0 :: CInt)
      vecSize = elemSize * size
  input <- mallocArray size :: IO (Ptr CInt)
  let cbuf = getGpuBuffer d gpub
  evt <- clEnqueueReadBuffer (queue d) cbuf True 0 vecSize (castPtr input) (waitlist d)
  -- send the read contents to the convertor in d
  r <- convertor d size input
  returnAction (d {waitlist = [evt]}) $ Just r
runAction d (ReadBuffer bufdata gpub@(CLGPUBuffer _ size)) = do
  let elemSize = sizeOf (0 :: CInt)
      vecSize = elemSize * size
  let cbuf = getGpuBuffer d gpub
  mem_in <- clCreateBuffer (context d) [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (vecSize, castPtr bufdata)
  -- send the read contents to the convertor in d
  returnAction (d {gpuBuffers = (gpub, mem_in) : gpuBuffers d}) Nothing

-- | Update the runner and return value
returnAction :: RunCLData a -> Maybe a -> IO (OpenCLRunner a)
returnAction v r = return $ OpenCLRunner (runAction v) r
