module OpenCL (OpenCLAction(..)) where

data OpenCLAction =
    EnqueueKernel String Range | MakeKernel String :w
