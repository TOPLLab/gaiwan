module CodeGen.OpenCL (mkOpenCLKernelCode) where

import CodeGen.CLike
import Language.GaiwanDefs

mkOpenCLKernelCode a@(PipedExp expressions) = mkCodeB a
mkOpenCLKernelCode a = mkCode a
