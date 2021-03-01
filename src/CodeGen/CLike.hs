module CodeGen.CLike where

import Code.SCode
import Language.GaiwanDefs
import CodeGen.Pipelining

mkBinOp a b op = do
  ka <- mkCodeB a
  kb <- mkCodeB b
  return $ ka ++ op ++ kb

mkCode :: Exp -> SCode String
mkCode Let {} = error "Let not yet supported!"
mkCode (Plus a b) = mkBinOp a b "+"
mkCode (Minus a b) = mkBinOp a b "-"
mkCode (Times a b) = mkBinOp a b "*"
mkCode (Div a b) = mkBinOp a b "/"
mkCode (Modulo a b) = mkBinOp a b "%"
mkCode (Pow a b) = mkBinOp a b "^"
mkCode (IsEq a b) = mkBinOp a b "=="
mkCode (IsGreater a b) = mkBinOp a b "<"
mkCode (If cond texp fexp) = do
  condStr <- mkCodeB cond
  restultStr <- mkBinOp texp fexp ":"
  return $ condStr ++ "?" ++ restultStr
mkCode (App f builtin args) = do
  callKind <- lookupDef f
  case callKind of
    Just stmta ->
      stmt
        stmta
        ( \_ argNames bodys ->
            mkCodeB $
              substMult
                (zip ((`Var` False) <$> argNames) args)
                (head bodys)
        )
mkCode (Int num) = return $ show num
mkCode (Var name True) = return $ "int_" ++ name
mkCode (Var name False) = return $ "arg_" ++ name
mkCode (Negate x) = ("-" ++) <$> mkCodeB x
mkCode (ArrayGet x idx) = do
  kx <- mkCodeB x
  kidx <- mkCodeB idx
  return $ kx ++ "[" ++ kidx ++ "]"
mkCode (PipedExp expressions) = do
  convertPls mkCodeB expressions
  return ""
mkCode unknownCode = error $ "Could not convert code:" ++ show unknownCode

-- Add brackets
mkCodeB :: Exp -> SCode String
mkCodeB x = do
  v <- mkCode x
  return $ "(" ++ v ++ ")"
