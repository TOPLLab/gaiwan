module Code
  ( SCode,
    KernelName (),
    addDeviceCode,
    addDeviceKernel,
    addKernelCall,
    addHostCode,
    dbgRender,
    execCode,
    lookupDef,
    registerDef,
    runCode,
  )
where

import Control.Monad.State.Lazy
import Data.Functor
import Data.List
import Language.Gaiwan
import OpenCL

data Code = Code
  { deviceCode :: String,
    hostCode :: [OpenCLAction],
    nameCount :: Int,
    defs :: [Stmt],
    kernels :: [((Exp, [GPUBuffer], [GPUBuffer]), KernelName)] -- Put kernels here
  }
  deriving (Show)

type SCode a = State Code a

newtype KernelName = KernelName String deriving (Show)

kernelName (KernelName n) = n

runCode :: Code -> IO ()
runCode c = do
  oclr <- runList (mkOpenRunner (deviceCode c)) (hostCode c)
  str <- showOpenRunner oclr
  putStrLn str
  return ()

dbgRender :: Code -> String
dbgRender c = show (hostCode c) ++ "\n\n" ++ deviceCode c

emptyCode =
  Code
    { deviceCode = "",
      hostCode = [],
      nameCount = 0,
      defs = [],
      kernels = []
    }

execCode :: SCode a -> Code
execCode s = execState s emptyCode

getAName :: String -> State Code KernelName
getAName prefix = do
  old@Code {nameCount = nc} <- get
  put old {nameCount = nc + 1}
  return $ KernelName $ prefix ++ show nc

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
addHostCode :: OpenCLAction -> SCode ()
addHostCode s =
  modify
    ( \old@Code {hostCode = dc} -> old {hostCode = dc ++ [s]}
    )

addDeviceCode :: String -> SCode ()
addDeviceCode s =
  modify
    ( \old@Code {deviceCode = dc} ->
        old {deviceCode = dc ++ "\n\n" ++ s ++ "\n"}
    )

-- Adds a kernel and retrns the name
addDeviceKernel :: Exp -> [GPUBuffer] -> [GPUBuffer] -> String -> SCode KernelName
addDeviceKernel exp buffers buffersout code = do
  ks <- kernels <$> get
  maybe realyAddKernel return $ lookup ks
  where
    lookup :: [((Exp, [GPUBuffer], [GPUBuffer]), KernelName)] -> Maybe KernelName
    lookup ((h, n) : r) | matches h = Just n
    lookup (_ : r) = lookup r
    lookup [] = Nothing

    matches e = (exp, buffers, buffersout) == e -- todo make ignore names
    realyAddKernel :: SCode KernelName
    realyAddKernel = do
      name <- getAName "kernel"
      let KernelName strName = name
      addDeviceCode $
        "void kernel "
          ++ strName
          ++ "("
          ++ intercalate ", " (map gpuBufferDecl buffers)
          ++ "){ int int_index = get_global_id(0);"
          ++ intercalate ";" (map (\buffer@(GPUBuffer outname _) -> "int_" ++ outname ++ "[int_index] = " ++ code) buffersout)
          ++ ";};"
      registerKernel exp buffers buffersout name -- remember for next time
      return name

registerKernel :: Exp -> [GPUBuffer] -> [GPUBuffer] -> KernelName -> SCode ()
registerKernel exp buffers buffersout name =
  modify
    ( \old@Code {kernels = ks} -> old {kernels = ((exp, buffers, buffersout), name) : ks}
    )

gpuBufferDecl (GPUBuffer name size) =
  "global int int_" ++ name ++ "[" ++ show size ++ "]"

-- TODO: clean
addKernelCall :: KernelName -> [GPUBuffer] -> Range -> SCode ()
addKernelCall = ((addHostCode .) .) . MakeKernel . kernelName
