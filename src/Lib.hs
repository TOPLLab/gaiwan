module Lib
  ( someFunc,
    convert,
  )
where

import Code
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Language.Gaiwan
import Language.GaiwanDefs
-- import OpenCL
import Pipelining
import System.Exit

someFunc = someFunc2

someFunc1 :: String -> IO ()
someFunc1 = printG . parseGaiwan
  where
    printG (Left m) = do
      putStr "Parsing failed!\n"
      putStr m
      putStr "\n"
      exitFailure
    printG (Right m) = putStr $ show m

someFunc2 :: String -> IO ()
someFunc2 = printG . parseGaiwan
  where
    printG (Left m) = do
      putStr "Parsing failed!\n"
      putStr m
      putStr "\n"
      exitFailure
    printG (Right m) = do
      result <- convert m
      print result

someFunc3 :: String -> IO ()
someFunc3 = printG . parseGaiwan
  where
    printG (Left m) = do
      putStr "Parsing failed!\n"
      putStr m
      putStr "\n"
      exitFailure
    printG (Right m) = do
      print m
      print $ Code.compile $ Lib.compile m


compile (Prog defines main)=
    execCode $ do
      mapM_ registerDef defines
      convertMain main

convert :: Program -> IO [[Integer]]
convert program =
  runCodeToList $ Lib.compile program

funPrefix True = "int_"
funPrefix False = "user_"

mkKernelBinOp a b op = do
  ka <- mkKernelCodeB a
  kb <- mkKernelCodeB b
  return $ ka ++ op ++ kb

mkKernelCode :: Exp -> SCode String
mkKernelCode Let {} = error "Let not yet supported!"
mkKernelCode (Plus a b) = mkKernelBinOp a b "+"
mkKernelCode (Minus a b) = mkKernelBinOp a b "-"
mkKernelCode (Times a b) = mkKernelBinOp a b "*"
mkKernelCode (Div a b) = mkKernelBinOp a b "/"
mkKernelCode (Modulo a b) = mkKernelBinOp a b "%"
mkKernelCode (Pow a b) = mkKernelBinOp a b "^"
mkKernelCode (IsEq a b) = mkKernelBinOp a b "=="
mkKernelCode (IsGreater a b) = mkKernelBinOp a b "<"
mkKernelCode (If cond texp fexp) = do
    condStr <- mkKernelCodeB cond
    restultStr <- mkKernelBinOp texp fexp ":"
    return $ condStr ++ "?"  ++ restultStr

mkKernelCode (App f builtin args) = do
  callKind <- lookupDef f
  case callKind of
    Just stmta ->
      stmt
        stmta
        ( \_ argNames bodys ->
            mkKernelCodeB $
              substMult
                (zip ((`Var` False) <$> argNames) args)
                (head bodys)
        )
mkKernelCode (Int num) = return $ show num
mkKernelCode (Var name True) = return $ "int_" ++ name
mkKernelCode (Var name False) = return $ "arg_" ++ name
mkKernelCode (Negate x) = ("-" ++) <$> mkKernelCodeB x
mkKernelCode (ArrayGet x idx) = do
  kx <- mkKernelCodeB x
  kidx <- mkKernelCodeB idx
  return $ kx ++ "[" ++ kidx ++ "]"
mkKernelCode (PipedExp expressions) = do
  convertPls mkKernelCodeB expressions
  return ""
mkKernelCode unknownCode = error $ "Could not convert code:" ++ show unknownCode


-- Add brackets
mkKernelCodeB :: Exp -> SCode String
mkKernelCodeB x = do
  v <- mkKernelCode x
  return $ "(" ++ v ++ ")"

-- Convert main
convertMain :: Exp -> SCode ()
convertMain main =
  void $ mkKernelCode main

-- todo make less ugly
