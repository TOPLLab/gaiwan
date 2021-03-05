module Code.SCode
  ( Code (..),
    SCode (),
    addDeviceCode,
    addHostCode,
    registerKernel,
    freshKernelName,
    getKernels,
    lookupDef,
    registerDef,
    execCode,
    freshGPUBuffer,
  )
where

import Code.Definitions
import Control.Monad
import Control.Monad.State.Lazy
import Data.Functor
import Data.List as L hiding (delete, insert, union)
import Language.GaiwanDefs

type SCode b a = State (Code b) a

data Code a = Code
  { deviceCode :: a, -- Kernel definitions
    hostCode :: [GPUAction], -- Plan for the host
    nameCount :: Int, -- For unique kernel names
    defs :: [Stmt], -- Defnitions to be used in the program
    kernels :: [(([Exp], [GPUBuffer], [GPUBuffer]), KernelName)], -- Put kernels here
    bufferCount :: Int -- For unique buffer names
  }
  deriving (Show)

execCode :: (Monoid b) => SCode b a -> Code b
execCode s =
  execState
    s
    Code
      { deviceCode = mempty,
        hostCode = [],
        nameCount = 0,
        defs = [],
        kernels = [],
        bufferCount = 0 -- mkCode should be here
      }

freshGPUBuffer :: Int -> SCode b GPUBuffer
freshGPUBuffer size = do
  name <- freshGPUBufferName
  return $ GPUBuffer name size

freshGPUBufferName :: SCode b GPUBufferName
freshGPUBufferName = do
  old@Code {bufferCount = nc} <- get
  put old {bufferCount = nc + 1}
  return $ GPUBufferName nc

registerDef :: Stmt -> SCode b ()
registerDef s = modify (\old@Code {defs = d} -> old {defs = s : d})

lookupDef :: String -> SCode b (Maybe Stmt)
lookupDef name = get <&> (lookup . defs)
  where
    lookup (r : _) | name == stmtName r = Just r
    lookup (_ : rest) = lookup rest
    lookup [] = Nothing

-- todo: add opencl calls
addHostCode :: GPUAction -> SCode b ()
addHostCode s =
  modify
    ( \old@Code {hostCode = dc} -> old {hostCode = dc ++ [s]}
    )

addDeviceCode :: (Monoid b) => b -> SCode b ()
addDeviceCode s =
  modify
    ( \old@Code {deviceCode = dc} ->
        old {deviceCode = dc <> s}
    )

freshKernelName :: State (Code a) KernelName
freshKernelName = do
  old@Code {nameCount = nc} <- get
  put old {nameCount = nc + 1}
  return $ KernelName $ "kernel" ++ show nc

registerKernel :: [Exp] -> [GPUBuffer] -> [GPUBuffer] -> KernelName -> SCode b ()
registerKernel exps buffers buffersout name =
  modify
    ( \old@Code {kernels = ks} -> old {kernels = ((exps, buffers, buffersout), name) : ks}
    )

getKernels :: SCode b [(([Exp], [GPUBuffer], [GPUBuffer]), KernelName)]
getKernels = kernels <$> get
