module CodeGen.OpenCL
  ( mkOpenCLKernelCode,
    runOpenCL,
    runOpenCLCompiled,
    runOpenCLWithConv,
    runOpenCLCompiledWithConv,
  )
where

import Code
import Code.Definitions as C
import Code.SCode
import CodeGen.CLike
import qualified Data.ByteString.Lazy as BS
import Foreign
import Foreign.C
import Language.GaiwanDefs
import OpenCL

mkOpenCLKernelCode a@(PipedExp expressions) = mkCodeB a

runOpenCL :: Code String -> IO [[Integer]]
runOpenCL c = uncurry runToList (compile c)

runOpenCLWithConv c s =
  maybe
    (return Nothing)
    (fmap Just . uncurry (runToListWithConvertor c))
    $ deserialize s

runOpenCLCompiled :: BS.ByteString -> IO (Maybe [[Integer]])
runOpenCLCompiled = runOpenCLCompiledWithConv cnftr

runOpenCLCompiledWithConv :: (Int -> Ptr CInt -> IO a) -> BS.ByteString -> IO (Maybe [a])
runOpenCLCompiledWithConv c s =
  maybe
    (return Nothing)
    (fmap Just . uncurry (runToListWithConvertor c))
    $ deserialize s

runToListWithConvertor :: (Int -> Ptr CInt -> IO a) -> String -> [GPUAction] -> IO [a]
runToListWithConvertor c devCode hostCode = do
  runner <- mkOpenRunner c devCode
  run runner $ toOpenCL hostCode

runToList :: String -> [GPUAction] -> IO [[Integer]]
runToList = runToListWithConvertor cnftr

-- | Convertor function from a malloed buffer to output,
--
-- Here, we take the numbers and convert them to ingegers
cnftr :: Int -> Ptr CInt -> IO [Integer]
cnftr size ptr = map toInteger <$> peekArray size ptr

toOpenCL :: [GPUAction] -> [OpenCLAction]
toOpenCL actions = addFrees $ map toOpenCLAction actions

-- | Convert a GPUBuffer to and OpenCLBuffer
toOpenCLBuf (GPUBuffer (GPUBufferName i) size) = CLGPUBuffer i size

-- | Convet actions tot OpenCL actions
toOpenCLAction :: GPUAction -> OpenCLAction
toOpenCLAction (CallKernel (KernelName n) args _ threads) = MakeKernel n (map toOpenCLBuf args) (Range threads 0 0)
toOpenCLAction (C.ReadBuffer x) = OpenCL.ReadBuffer (toOpenCLBuf x)
toOpenCLAction (C.AllocBuffer b) = OpenCL.AllocBuffer $ toOpenCLBuf b

-- | Add a FreeBuffer for every AllocBuffer action
addFrees l =
  l
    ++ foldr
      ( \new a -> case new of
          OpenCL.AllocBuffer x -> OpenCL.FreeBuffer x : a
          _ -> a
      )
      []
      l
