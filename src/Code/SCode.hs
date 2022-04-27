module Code.SCode
  ( Code (..),
    SCode (),
    addDeviceCode,
    addHostCode,
    addHostReadBuffer,
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
import Language.GaiwanTypes

type SCode b a = State (Code b) a

data Code a = Code
  { deviceCode :: a, -- Kernel definitions
    hostCode :: [GPUAction], -- Plan for the host
    nameCount :: Int, -- For unique kernel names
    defs :: [TypedTransform], -- Defnitions to be used in the program
    kernels :: [(([BExp], [ReservedBuffer], [ReservedBuffer]), KernelName)], -- Put kernels here
    bufferCount :: Int, -- For unique buffer names
    readBuffers :: [(String, ReservedBuffer)] -- mapping of already read buffers by their inport namew
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
        bufferCount = 0, -- mkCode should be here
        readBuffers = []
      }

freshGPUBuffer :: GaiwanBuf Int -> SCode b ReservedBuffer
freshGPUBuffer buf@(GaiwanBuf size shape) = do
  name <- freshGPUBufferName
  return $ ReservedBuffer name buf

freshGPUBufferName :: SCode b GPUBufferName
freshGPUBufferName = do
  old@Code {bufferCount = nc} <- get
  put old {bufferCount = nc + 1}
  return $ GPUBufferName nc

registerDef :: TypedTransform -> SCode b ()
registerDef s = modify (\old@Code {defs = d} -> old {defs = s : d})

lookupDef :: String -> SCode b (Maybe TypedTransform)
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

addHostReadBuffer :: String -> GaiwanBuf Int -> SCode b ReservedBuffer
addHostReadBuffer name buf = do
  b <- freshGPUBuffer buf
  addHostCode (ReadBuffer name b)
  return b

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

registerKernel :: [BExp] -> [ReservedBuffer] -> [ReservedBuffer] -> KernelName -> SCode b ()
registerKernel exps buffers buffersout name =
  modify
    ( \old@Code {kernels = ks} -> old {kernels = ((exps, buffers, buffersout), name) : ks}
    )

getKernels :: SCode b [(([BExp], [ReservedBuffer], [ReservedBuffer]), KernelName)]
getKernels = kernels <$> get
