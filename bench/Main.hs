{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad.State (evalStateT)
import CodeGen.Pipelining (prepare)
import Criterion.Main
import qualified Data.ByteString.Lazy as BS
import Data.Either
import Foreign
import Foreign.Storable
import Language.Gaiwan
import Language.GaiwanDefs
import Language.GaiwanTypes (backPropagate, checkType, TypedProgram)
import Lib
import OpenCL
import System.Environment.Blank
import Lib (convertTyped)

main :: IO ()
main = do
  let sizes = [8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432] :: [Int]
  let p = \s ->
        Prog
          [ Abstraction Nothing "bitonic_select" [("round", Just (AShape GaiwanInt)), ("arrPerBlock", Just (AShape GaiwanInt))] [Shaper (Just (ABuf (GaiwanBuf (GaiwanBufSize "n" 1 0) (GaiwanTuple [TVar "C", TVar "C"])))) "split" [("i", Nothing), ("d", Just (ABuf (GaiwanBuf (GaiwanBufSize "n" 2 0) (TVar "C"))))] (Let "blockid" (Div (Var "i" False) (Var "arrPerBlock" False)) (Let "blockstart" (Times (Times (Var "blockid" False) (Var "arrPerBlock" False)) (Int 2)) (Let "blockoffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "pos" (Plus (Var "blockstart" False) (Var "blockoffset" False)) (Tuple [ArrayGet (Var "d" False) (Var "pos" False), ArrayGet (Var "d" False) (Plus (Var "pos" False) (Var "arrPerBlock" False))]))))), Mapper (Just (AShape (GaiwanTuple [GaiwanInt, GaiwanInt]))) "bitonic_select_impl" [("i", Nothing), ("a", Just (AShape (GaiwanTuple [GaiwanInt, GaiwanInt])))] (If (IsGreater (Modulo (Var "i" False) (Pow (Int 2) (Plus (Var "round" False) (Int 1)))) (Pow (Int 2) (Var "round" False))) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Var "a" False) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0])) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0]) (Var "a" False))), Shaper (Just (ABuf (GaiwanBuf (GaiwanBufSize "n" 2 0) (TVar "B")))) "join" [("i", Nothing), ("d", Just (ABuf (GaiwanBuf (GaiwanBufSize "n" 1 0) (GaiwanTuple [TVar "B", TVar "B"]))))] (Let "arrowBlock" (Div (Var "i" False) (Times (Int 2) (Var "arrPerBlock" False))) (Let "arrowBlockStart" (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Let "arrowOffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (If (IsGreater (Plus (Times (Var "arrowBlockStart" False) (Int 2)) (Var "arrPerBlock" False)) (Plus (Var "i" False) (Int 1))) (Select (ArrayGet (Var "d" False) (Plus (Var "arrowBlockStart" False) (Var "arrowOffset" False))) 0) (Select (ArrayGet (Var "d" False) (Plus (Var "arrowBlockStart" False) (Var "arrowOffset" False))) 1)))))]
          ]
          [Return ["size" ++ show s], Loop (Int $ ((round $ logBase 2 (fromIntegral s)) :: Int)) "round" [Loop (Plus (Var "round" False) (Int 1)) "step" [IApp "bitonic_select" False [Var "round" False, Pow (Int 2) (Minus (Var "round" False) (Var "step" False))]]]]

  let !tps =
        map
          ( \s -> do
              !tp <- checkType (p s)
              !tpb <- evalStateT (backPropagate tp) 0
              let !(!code, !actions) = prepare tpb
              return (code,actions)
          )
          sizes
  let !tpsj = map rightOrError tps
  let !benchIn  = zip tpsj sizes
  mapM (print . show . length . show) tpsj

  -- Bang means strict evaluation
  -- We need to ensure that this is not counted in the benchmark
  defaultMain
    [ bgroup
        "sort"
        ( map
            ( \(!(code,actions), !s) ->
                ( bench ("test " ++ show s) $
                     nfIO $ convertPrepared code actions
                )
            )
            benchIn
        )
    ]

rightOrError :: Either String (a,b) -> (a,b)
rightOrError (Left s) = error $ "failed to type prog " ++ s
rightOrError !(Right !(!a,!b)) = (a,b)
