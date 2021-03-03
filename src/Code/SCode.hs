module Code.SCode
  ( Code (..),
    SCode (),
    addDeviceCode,
    addHostCode,
    registerKernel,
    freshKernelName,
    getKernels,
    deviceCodeStr,
    lookupDef,
    registerDef,
    execCode,
    freshGPUBuffer,
  )
where

import Code.Definitions
import Control.Monad.State.Lazy
import Data.Functor
import Data.List as L hiding (delete, insert, union)
import Language.GaiwanDefs

type SCode a = State Code a

data Code = Code
  { deviceCode :: [String], -- Kernel definitions
    hostCode :: [GPUAction], -- Plan for the host
    nameCount :: Int, -- For unique kernel names
    defs :: [Stmt], -- Defnitions to be used in the program
    kernels :: [(([Exp], [GPUBuffer], [GPUBuffer]), KernelName)], -- Put kernels here
    bufferCount :: Int -- For unique buffer names
  }
  deriving (Show)

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


registerDef :: Stmt -> SCode ()
registerDef s = modify (\old@Code {defs = d} -> old {defs = s : d})

lookupDef :: String -> SCode (Maybe Stmt)
lookupDef name = get <&> (lookup . defs)
  where
    lookup (r : _) | name == stmtName r = Just r
    lookup (_ : rest) = lookup rest
    lookup [] = Nothing

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

deviceCodeStr :: Code -> String
deviceCodeStr = intercalate "\n\n" . reverse . deviceCode



freshKernelName :: State Code KernelName
freshKernelName = do
  old@Code {nameCount = nc} <- get
  put old {nameCount = nc + 1}
  return $ KernelName $ "kernel" ++ show nc

registerKernel :: [Exp] -> [GPUBuffer] -> [GPUBuffer] -> KernelName -> SCode ()
registerKernel exps buffers buffersout name =
  modify
    ( \old@Code {kernels = ks} -> old {kernels = ((exps, buffers, buffersout), name) : ks}
    )

getKernels :: SCode [(([Exp], [GPUBuffer], [GPUBuffer]), KernelName)]
getKernels = kernels <$> get


