{-# LANGUAGE MultiParamTypeClasses #-}

module Code.Definitions
  ( GPUBufferName (..),
    GPUBuffer (..),
    GPUAction (..),
    gpuBufferSize,
    KernelName (..),
  )
where

newtype GPUBufferName = GPUBufferName Int deriving (Show, Ord, Eq)

data GPUBuffer = GPUBuffer GPUBufferName Int deriving (Show, Ord, Eq)

gpuBufferSize (GPUBuffer _ s) = s

newtype KernelName = KernelName String deriving (Show, Eq)

data GPUAction
  = CallKernel KernelName [GPUBuffer] [GPUBuffer] Int -- name args threads
  | ReadBuffer GPUBuffer
  | AllocBuffer GPUBuffer
  deriving (Show, Eq)
