module Code
  ( SCode,
    KernelName (),
    GPUAction (..),
    GPUBuffer (..),
    addDeviceCode,
    addDeviceKernel,
    addHostCode,
    gpuBufferGet,
    dbgRender,
    execCode,
    lookupDef,
    registerDef,
    freshGPUBuffer,
    runCodeToList,
    gpuBufferSize,
  )
where

import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Functor
import Data.List
import Data.Maybe
import Language.Gaiwan
import Language.GaiwanDefs
import OpenCL

newtype GPUBufferName = GPUBufferName Int deriving (Show, Ord, Eq)

data GPUBuffer = GPUBuffer GPUBufferName Int deriving (Show, Eq, Ord)

gpuBufferSize (GPUBuffer _ s) = s

data GPUAction
  = CallKernel KernelName [GPUBuffer] Int -- name args threads
  | AllocBuffer GPUBuffer
  | ReadBuffer GPUBuffer
  deriving (Show)

data Code = Code
  { deviceCode :: [String],
    hostCode :: [GPUAction],
    nameCount :: Int,
    defs :: [Stmt],
    kernels :: [(([Exp], [GPUBuffer], [GPUBuffer]), KernelName)], -- Put kernels herem
    bufferCount :: Int
  }
  deriving (Show)

deviceCodeStr :: Code -> String
deviceCodeStr = intercalate "\n\n" . reverse . deviceCode

type SCode a = State Code a

newtype KernelName = KernelName String deriving (Show)

toOpenCL :: GPUAction -> OpenCLAction
toOpenCL (CallKernel (KernelName n) args threads) = MakeKernel n (map toOpenCLBuf args) (Range threads 0 0)
toOpenCL (Code.AllocBuffer x) = OpenCL.AllocBuffer (toOpenCLBuf x)
toOpenCL (Code.ReadBuffer x) = OpenCL.ReadBuffer (toOpenCLBuf x)

toOpenCLBuf (GPUBuffer (GPUBufferName i) size) = CLGPUBuffer i size

runCodeToList :: Code -> IO [[Integer]]
runCodeToList c = do
  runner <- mkOpenRunnerInteger (deviceCodeStr c)
  run runner (map toOpenCL $ hostCode c)

dbgRender :: Code -> String
dbgRender c = show (hostCode c) ++ "\n\n" ++ deviceCodeStr c

emptyCode =
  Code
    { deviceCode = [],
      hostCode = [],
      nameCount = 0,
      defs = [],
      kernels = [],
      bufferCount = 0 -- mkCode should be here
    }

execCode :: SCode a -> Code
execCode s = execState s emptyCode

freshGPUBuffer :: Int -> SCode GPUBuffer
freshGPUBuffer size = do
  name <- freshGPUBufferName
  return $ GPUBuffer name size

freshGPUBufferName :: SCode GPUBufferName
freshGPUBufferName = do
  old@Code {bufferCount = nc} <- get
  put old {bufferCount = nc + 1}
  return $ GPUBufferName nc

freshKernelName :: State Code KernelName
freshKernelName = do
  old@Code {nameCount = nc} <- get
  put old {nameCount = nc + 1}
  return $ KernelName $ "kernel" ++ show nc

registerDef :: Stmt -> SCode ()
registerDef s = modify (\old@Code {defs = d} -> old {defs = s : d})

lookupDef :: String -> SCode (Maybe Stmt)
lookupDef name = get <&> (lookup . defs)
  where
    lookup (r : _) | name == defName r = Just r
    lookup (_ : rest) = lookup rest
    lookup [] = Nothing

defName (Mapper name _ _) = name
defName (Shuffler name _ _) = name

-- todo: add opencl calls
addHostCode :: GPUAction -> SCode ()
addHostCode s =
  modify
    ( \old@Code {hostCode = dc} -> old {hostCode = dc ++ [s]}
    )

addDeviceCode :: String -> SCode ()
addDeviceCode s =
  modify
    ( \old@Code {deviceCode = dc} ->
        old {deviceCode = s : dc}
    )

-- Adds a kernel and retrns the name
-- Creates a kernel that sets the output of the i-th expression to the i-th output buffer
addDeviceKernel :: (Exp -> SCode String) -> [Exp] -> [GPUBuffer] -> [GPUBuffer] -> SCode KernelName
addDeviceKernel mkCode initExps initBuffers initBuffersout = do
  ks <- kernels <$> get
  maybe realyAddKernel return $ lookup ks
  where
    (exps, buffers, buffersout) = canonicalKernel (initExps, initBuffers, initBuffersout)
    lookup :: [(([Exp], [GPUBuffer], [GPUBuffer]), KernelName)] -> Maybe KernelName
    lookup ((h, n) : r) | matches h = Just n
    lookup (_ : r) = lookup r
    lookup [] = Nothing

    matches e = (exps, buffers, buffersout) == e -- todo make ignore names
    realyAddKernel :: SCode KernelName
    realyAddKernel = do
      name <- freshKernelName
      code <- mapM mkCode exps
      addDeviceCode $
        mkKernelShell name buffers $
          " int int_index = get_global_id(0);\n"
            ++ intercalate "\n" (zipWith (gpuBufferAssign "int_index") buffersout code)
      registerKernel exps buffers buffersout name -- remember for next time
      return name

canonicalKernel :: ([Exp], [GPUBuffer], [GPUBuffer]) -> ([Exp], [GPUBuffer], [GPUBuffer])
canonicalKernel (e, buffers, buffersOut) = (map (simpleSubstMult requiredSubst) e, translatedBuffers, translatedBuffersOut)
  where
    canonicalBuffers = canonicalBufferKV $ buffers ++ buffersOut
    translatedBuffers = map translateBuffer buffers
    translatedBuffersOut = map translateBuffer buffersOut
    translateBuffer x = fromJust $ lookup x canonicalBuffers
    requiredSubst :: [(Exp, Exp)]
    requiredSubst = map (bimap gpuBufferVar gpuBufferVar) canonicalBuffers

canonicalBufferKV :: [GPUBuffer] -> [(GPUBuffer, GPUBuffer)]
canonicalBufferKV buffers = removeDoubles $ zip buffers $ canonicalBufferNames buffers

removeDoubles [] = []
removeDoubles ((k, v) : r) = (k, v) : removeDoubles (filter (\(ko, _) -> ko /= k) r)

canonicalBufferNames :: [GPUBuffer] -> [GPUBuffer]
canonicalBufferNames buffers = zipWith (\a@(GPUBuffer _ s) newname -> GPUBuffer newname s) buffers $ GPUBufferName <$> [0 ..]

mkKernelShell :: KernelName -> [GPUBuffer] -> String -> String
mkKernelShell (KernelName name) args code = "void kernel " ++ name ++ "(" ++ argsStr ++ ")" ++ "{ \n" ++ code ++ " \n};"
  where
    argsStr = intercalate ", " (map gpuBufferDecl args)

registerKernel :: [Exp] -> [GPUBuffer] -> [GPUBuffer] -> KernelName -> SCode ()
registerKernel exps buffers buffersout name =
  modify
    ( \old@Code {kernels = ks} -> old {kernels = ((exps, buffers, buffersout), name) : ks}
    )

gpuBufferDecl gpub@(GPUBuffer _ size) =
  "global int " ++ gpuBufferArgName gpub ++ "[" ++ show size ++ "]"

gpuBufferAssign :: String -> GPUBuffer -> String -> String
gpuBufferAssign index buffer value = gpuBufferArgName buffer ++ "[" ++ index ++ "] = " ++ value ++ ";"

-- TODO make the stuff below nicer
gpuBufferGet :: GPUBuffer -> Exp -> Exp
gpuBufferGet buffer = ArrayGet (gpuBufferVar buffer)

gpuBufferVar (GPUBuffer (GPUBufferName name) _) = Var ("array" ++ show name) True

gpuBufferArgName (GPUBuffer (GPUBufferName name) _) = "int_array" ++ show name
