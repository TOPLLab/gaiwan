{-# LANGUAGE MultiParamTypeClasses #-}

module Code.Definitions
  ( GPUBufferName (..),
    GPUAction (..),
    ReservedBuffer (..),
    KernelName (..),
    BExp (..),
  )
where

import Language.GaiwanDefs

type BExp = GExp ReservedBuffer -- Expression that can use GPUBufferGet (ReservedBuffer) BExp

-- Names for buffers
newtype GPUBufferName = GPUBufferName Int deriving (Show, Ord, Eq)

data Void -- Empty datatype

newtype KernelName = KernelName String deriving (Show, Eq)

data ReservedBuffer = ReservedBuffer GPUBufferName (GaiwanBuf Int)
  deriving (Show, Eq)

-- TODO derive this one
instance Ord ReservedBuffer where
  compare
    (ReservedBuffer gbn (GaiwanBuf (GaiwanBufSize x y z) gs))
    (ReservedBuffer gbn' (GaiwanBuf (GaiwanBufSize n i j) gs')) =
      compare (gbn, x, y, z, gs) (gbn', n, i, j, gs')

data GPUAction
  = CallKernel KernelName [ReservedBuffer] [ReservedBuffer] -- name args resultloc
  | ReadBuffer String ReservedBuffer -- Write the contents of a named buffer into a GPU buffer
  | AllocBuffer ReservedBuffer
  | OutputBuffer [ReservedBuffer] -- Extract a list of buffers from the device to the host
  | Infoz String
  deriving (Show, Eq)
