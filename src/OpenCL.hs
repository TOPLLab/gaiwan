module OpenCL
    ( OpenCLAction(..)
    , GPUBuffer(..)
    , Range(..)
    )
where

import Control.Parallel.OpenCL

data GPUBuffer = GPUBuffer String Int deriving (Show,Eq,Ord)
data Range = Range Int Int Int deriving (Show)

data OpenCLAction =
    MakeKernel String [GPUBuffer] Range
    | AllocBuffer GPUBuffer
    | ReadBuffer GPUBuffer
    deriving (Show)

runAction :: CLContext -> CLCommandQueue-> CLProgram -> OpenCLAction -> IO ()
runAction c q p (MakeKernel name args range) = do
    kernel <- clCreateKernel p name
    mapM_ (\(i,n)->clSetKernelArgSto kernel i $ getBuf n) (zip [0..] args)
    clEnqueueNDRangeKernel q kernel RANGE
runAction c q p a= error (show a)

getBuf :: GPUBuffer -> CLMem
getBuf = error . show
