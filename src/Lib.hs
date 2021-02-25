module Lib
  ( run,
    runCompiled,
    Lib.compile,
    checkParse,
    convert,
  )
where

import Code
-- import OpenCL

import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe
import Language.Gaiwan
import Language.GaiwanDefs
import Pipelining
import System.Exit

someFunc = Lib.compile

eitherParseGaiwan a b s = either a b $ parseGaiwan s

run :: String -> IO ()
run =
  eitherParseGaiwan
    (\err -> print $ "Parsing failed: " ++ err)
    (convert >=> print)

checkParse :: String -> Either String Program
checkParse =
  eitherParseGaiwan
    (\err -> Left $ "Parsing failed: " ++ err)
    Right

-- Output a bytestring
compile :: String -> Either String BS.ByteString
compile =
  eitherParseGaiwan
    (\err -> Left $ "Parsing failed: " ++ err)
    (Right . uncurry Code.serialize . Code.compile . mkCode) -- todo make either

mkCode (Prog defines main) =
  execCode $ do
    mapM_ registerDef defines
    convertMain main

convert :: Program -> IO [[Integer]]
convert program =
  runCodeToList $ mkCode program

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
  return $ condStr ++ "?" ++ restultStr
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
