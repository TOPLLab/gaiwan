module Lib
  ( someFunc,
  )
where

import Code
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import Debug.Trace
import Language.Gaiwan
import Language.GaiwanDefs
-- import OpenCL
import Pipelining
import System.Exit


traceThisNote :: (Show a) => String -> a -> a
traceThisNote note a = trace (note ++ ":" ++ show a) a

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
    printG (Right m) = convert m

convert :: Program -> IO ()
convert (Prog defines main) =
  runCode $
    execCode $ do
      mapM_ registerDef defines
      convertMain main

funPrefix True = "int_"
funPrefix False = "user_"

mkKernelBinOp a b op = do
  ka <- mkKernelCodeB a
  kb <- mkKernelCodeB b
  return $ ka ++ op ++ kb

mkKernelCode :: Exp -> SCode String
mkKernelCode Let {} = error "Let not yet supported!"
mkKernelCode (Plus (Int a) (Int b)) = mkKernelCodeB (Int (a + b))
mkKernelCode (Plus (Int 0) b) = mkKernelCodeB b -- move to simplify
mkKernelCode (Plus b (Int 0)) = mkKernelCodeB b
mkKernelCode (Plus a b) = mkKernelBinOp a b "+"
mkKernelCode (Minus a b) = mkKernelBinOp a b "-"
mkKernelCode (Times a b) = mkKernelBinOp a b "*"
mkKernelCode (Div a b) = mkKernelBinOp a b "/"
mkKernelCode (Modulo a b) = mkKernelBinOp a b "%"
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
