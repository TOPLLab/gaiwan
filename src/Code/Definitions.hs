{-# LANGUAGE MultiParamTypeClasses #-}

module Code.Definitions
  ( GPUBufferName (..),
    GPUAction (..),
    ReservedBuffer (..),
    KernelName (..),
    BExp (..),
    varFiller,
    reservedBufferId,
  )
where

import Data.Foldable (foldrM)
import Data.Map as M hiding (foldr)
import Language.GaiwanDefs

type BExp = GExp ReservedBuffer -- Expression that can use GPUBufferGet (ReservedBuffer) BExp

-- Names for buffers
newtype GPUBufferName = GPUBufferName Int deriving (Show, Ord, Eq)

newtype KernelName = KernelName String deriving (Show, Eq)

data ReservedBuffer = ReservedBuffer GPUBufferName (GaiwanBuf Int)
  deriving (Show, Eq)

reservedBufferId :: ReservedBuffer -> Int
reservedBufferId (ReservedBuffer (GPUBufferName n) gb) = n

-- TODO deal with shapesvars
varFiller :: (Ord a, Show a) => [(GaiwanBuf a, (GShape a, Int))] -> Either String (GaiwanBuf a -> Either String (GShape Void, Int))
varFiller mapping = do
  m <- sizeMap
  return $ result_ m
  where
    result_ sMap (GaiwanBuf (GaiwanBufSize n i j) GaiwanInt) = case M.lookup n sMap of
      Just multiplier -> Right $ (GaiwanInt, multiplier * i + j)
      Nothing -> Left "Could not find the letter in the sizemap"
    result_ sMap (GaiwanBuf (GaiwanBufSize n i j) _) = Left "non int buffers not yet supported"

    sizeMap = foldrM reservedSize M.empty mapping
    -- Determine the value of variables in a reserved buffer based on the actual size
    reservedSize
      (GaiwanBuf gbs@(GaiwanBufSize n i j) _, (_, actualSize))
      m =
        let (mult, rem) = (actualSize - j) `divMod` i
         in case (rem, M.lookup n m) of
              (0, Just oldMult) | oldMult == mult -> Right m
              (0, Just oldMult) -> Left "conflicting input sizes"
              (0, Nothing) -> Right $ M.insert n mult m
              v -> Left $ "Invalid input size! " ++ show (gbs, actualSize, v)

-- TODO derive this one
instance Ord ReservedBuffer where
  compare
    (ReservedBuffer gbn (GaiwanBuf (GaiwanBufSize x y z) gs))
    (ReservedBuffer gbn' (GaiwanBuf (GaiwanBufSize n i j) gs')) =
      compare (gbn, x, y, z, gs) (gbn', n, i, j, gs')

data GPUAction
  = CallKernel KernelName [ReservedBuffer] [ReservedBuffer] -- name args resultloc
  | CallReducerKernel KernelName [ReservedBuffer] ReservedBuffer -- one output buffer
  | CallAssocReducerKernel KernelName KernelName [ReservedBuffer] ReservedBuffer -- one output buffer
  | ReadBuffer String ReservedBuffer -- Write the contents of a named buffer into a GPU buffer
  | AllocBuffer ReservedBuffer
  | OutputBuffer [ReservedBuffer] -- Extract a list of buffers from the device to the host
  | Infoz String
  deriving (Show, Eq)
