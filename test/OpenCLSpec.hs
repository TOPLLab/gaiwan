module OpenCLSpec (spec) where

import qualified Code.Definitions
import CodeGen.Pipelining (prepare)
import Data.Map (fromList)
import Language.Gaiwan
import Language.GaiwanDefs
import Language.GaiwanTypes
import OpenCL (CLGPUBuffer (CLGPUBuffer), OpenCLAction (AllocBuffer, ExtractBuffer, MakeKernel, ReadBuffer), Range (Range), convertPlan)
import Test.Hspec
import Test.QuickCheck

(progCode, planning) = prepare $ TypedProg (GTransformType (fromList [("a", GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)]) [] [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt]) [TReturn (GTransformType (fromList [("a", GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)]) [] [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt]) ["a"], TLoop (GTransformType (fromList [("a", GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)]) [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt] [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt]) (Int 4) "round" [TLoop (GTransformType (fromList [("a", GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)]) [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt] [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt]) (Plus (Var "round" False) (Int 1)) "step" [TIApp (GTransformType (fromList [("a", GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)]) [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt] [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt]) (TAbstraction (GaiwanArrow [GaiwanInt, GaiwanInt] (GTransformType (fromList []) [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt] [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt])) "bitonic_select" ["round", "arrPerBlock"] [TShaper (GTransformType (fromList []) [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt] [GaiwanBuf (GaiwanBufSize 93 1 0) (GaiwanTuple [GaiwanInt, GaiwanInt])]) "split" ["i", "d"] (Let "blockid" (Div (Var "i" False) (Var "arrPerBlock" False)) (Let "blockstart" (Times (Times (Var "blockid" False) (Var "arrPerBlock" False)) (Int 2)) (Let "blockoffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "pos" (Plus (Var "blockstart" False) (Var "blockoffset" False)) (Tuple [ArrayGet (Var "d" False) (Var "pos" False), ArrayGet (Var "d" False) (Plus (Var "pos" False) (Var "arrPerBlock" False))]))))), TMapper (GTransformType (fromList []) [GaiwanBuf (GaiwanBufSize 93 1 0) (GaiwanTuple [GaiwanInt, GaiwanInt])] [GaiwanBuf (GaiwanBufSize 93 1 0) (GaiwanTuple [GaiwanInt, GaiwanInt])]) "bitonic_select_impl" ["i", "a"] (If (IsGreater (Modulo (Var "i" False) (Pow (Int 2) (Plus (Var "round" False) (Int 1)))) (Pow (Int 2) (Var "round" False))) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Var "a" False) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0])) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0]) (Var "a" False))), TShaper (GTransformType (fromList []) [GaiwanBuf (GaiwanBufSize 93 1 0) (GaiwanTuple [GaiwanInt, GaiwanInt])] [GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt]) "join" ["i", "d"] (Let "arrowBlock" (Div (Var "i" False) (Times (Int 2) (Var "arrPerBlock" False))) (Let "arrowBlockStart" (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Let "arrowOffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (If (IsGreater (Plus (Times (Var "arrowBlockStart" False) (Int 2)) (Var "arrPerBlock" False)) (Var "i" False)) (Select (ArrayGet (Var "d" False) (Plus (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Var "arrowOffset" False))) 0) (Select (ArrayGet (Var "d" False) (Plus (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Var "arrowOffset" False))) 1)))))]) [Var "round" False, Pow (Int 2) (Minus (Var "round" False) (Var "step" False))]]]]

demoPlan =
  [ Code.Definitions.AllocBuffer (Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 1) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)),
    Code.Definitions.AllocBuffer (Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 2) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)),
    Code.Definitions.ReadBuffer "a" (Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 0) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)),
    Code.Definitions.CallKernel (Code.Definitions.KernelName "kernel0") [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 0) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)] [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 1) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)],
    Code.Definitions.CallKernel (Code.Definitions.KernelName "kernel1") [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 1) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)] [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 2) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)],
    Code.Definitions.CallKernel (Code.Definitions.KernelName "kernel2") [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 2) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)] [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 1) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)],
    Code.Definitions.CallKernel (Code.Definitions.KernelName "kernel3") [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 1) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)] [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 2) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)],
    Code.Definitions.CallKernel (Code.Definitions.KernelName "kernel4") [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 2) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)] [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 1) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)],
    Code.Definitions.CallKernel (Code.Definitions.KernelName "kernel5") [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 1) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)] [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 2) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)],
    Code.Definitions.CallKernel (Code.Definitions.KernelName "kernel6") [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 2) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)] [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 1) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)],
    Code.Definitions.OutputBuffer [Code.Definitions.ReservedBuffer (Code.Definitions.GPUBufferName 1) (GaiwanBuf (GaiwanBufSize 93 2 0) GaiwanInt)]
  ]

spec = do
  describe "Code.OpenCL" $ do
    it "Has the right input" $
      do
        planning `shouldBe` demoPlan

    it "Transforms a sort program" $ do
      Right (v, defines) <- convertPlan demoPlan
      (map (\c -> case c of (ReadBuffer _ buf) -> MakeKernel "Read a buffer" [buf] (Range 0 0 0); a -> a) v) -- remove the pointer from the test
        `shouldBe` ( [ AllocBuffer (CLGPUBuffer 1 4),
                       AllocBuffer (CLGPUBuffer 2 4),
                       MakeKernel "Read a buffer" [CLGPUBuffer 0 4] (Range 0 0 0),
                       MakeKernel "KernelName \"kernel0\"" [CLGPUBuffer 0 4, CLGPUBuffer 1 4] (Range 4 0 0),
                       MakeKernel "KernelName \"kernel1\"" [CLGPUBuffer 1 4, CLGPUBuffer 2 4] (Range 4 0 0),
                       MakeKernel "KernelName \"kernel2\"" [CLGPUBuffer 2 4, CLGPUBuffer 1 4] (Range 4 0 0),
                       MakeKernel "KernelName \"kernel3\"" [CLGPUBuffer 1 4, CLGPUBuffer 2 4] (Range 4 0 0),
                       MakeKernel "KernelName \"kernel4\"" [CLGPUBuffer 2 4, CLGPUBuffer 1 4] (Range 4 0 0),
                       MakeKernel "KernelName \"kernel5\"" [CLGPUBuffer 1 4, CLGPUBuffer 2 4] (Range 4 0 0),
                       MakeKernel "KernelName \"kernel6\"" [CLGPUBuffer 2 4, CLGPUBuffer 1 4] (Range 4 0 0),
                       ExtractBuffer (CLGPUBuffer 1 4)
                     ] ::
                       [OpenCL.OpenCLAction]
                   )
