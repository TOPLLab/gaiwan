module Code.Definitions
  ( GPUBufferName (..),
    GPUBuffer (..),
    GPUAction (..),
    gpuBufferSize,
    KernelName (..),
  )
where

newtype GPUBufferName = GPUBufferName Int deriving (Show, Ord, Eq)

data GPUBuffer = GPUBuffer GPUBufferName Int deriving (Show, Eq, Ord)

gpuBufferSize (GPUBuffer _ s) = s

newtype KernelName = KernelName String deriving (Show)

data GPUAction
  = CallKernel KernelName [GPUBuffer] [GPUBuffer] Int -- name args threads
  | ReadBuffer GPUBuffer
  | AllocBuffer GPUBuffer
  deriving (Show)