{-# LANGUAGE MultiParamTypeClasses #-}

module Code.Definitions
  ( GPUBufferName (..),
    GPUBuffer (..),
    GPUAction (..),
    gpuBufferSize,
    KernelName (..),
    GShape (..),
    GShapeNoVar (..),
  )
where

-- Names for buffers
newtype GPUBufferName = GPUBufferName Int deriving (Show, Ord, Eq)

type GShapeNoVar = GShape Void

data Void -- Empty datatype

instance Eq Void where
  (==) _ _ = False

instance Ord Void where
  (<=) _ _ = False

instance Show Void where
  show _ = error "calling show on nonexisent value"

data GShape a
  = GaiwanInt -- TODO: float
  | GaiwanTuple [GShape a]
  | TVar a
  deriving (Show, Eq, Ord, Read)

-- A GPU buffer has a
-- - type of its contents
-- - Size
-- TODO: remove int
-- Note that a GPUBuffer of a product type (Tuple Int Int) may be implemeted as two "real" gpu buffers
data GPUBuffer = GPUBuffer GPUBufferName GShapeNoVar Int deriving (Show, Ord, Eq)

gpuBufferSize (GPUBuffer _ _ s) = s

newtype KernelName = KernelName String deriving (Show, Eq)

data GPUAction
  = CallKernel KernelName [GPUBuffer] [GPUBuffer] Int -- name args threads
  | ReadBuffer GPUBuffer
  | AllocBuffer GPUBuffer
  | Infoz String
  deriving (Show, Eq)
