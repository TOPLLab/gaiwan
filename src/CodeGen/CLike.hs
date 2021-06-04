module CodeGen.CLike where

import Code.Definitions
import Code.SCode
import CodeGen.Pipelining
import Data.List
import Language.GaiwanDefs

mkBinOp a b op = do
  ka <- mkCodeB a
  kb <- mkCodeB b
  return $ ka ++ op ++ kb

mkCode :: Exp -> SCode String String
mkCode Let {} = error "Let not yet supported!"
mkCode (Plus a b) = mkBinOp a b "+"
mkCode (Minus a b) = mkBinOp a b "-"
mkCode (Times a b) = mkBinOp a b "*"
mkCode (Div a b) = mkBinOp a b "/"
mkCode (Modulo a b) = mkBinOp a b "%"
mkCode (Pow (Int 2) b) = mkBinOp (Int 1) b "<<" -- TODO: what hapens if b is too large?
mkCode (Pow a b) = (\args -> "pow(" ++ args ++ ")") <$> mkBinOp a b ","
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
mkCode var@(Var _ _) = return $ varVarname var
mkCode (Negate x) = ("-" ++) <$> mkCodeB x
mkCode (ArrayGet x idx) = do
  kx <- mkCodeB x
  kidx <- mkCodeB idx
  return $ kx ++ "[" ++ kidx ++ "]"
mkCode (GPUBufferGet buffer idx) = mkCode (ArrayGet (gpuBufferVar buffer) idx)
mkCode (PipedExp expressions) = do
  convertPls True mkCodeB kernelTemplate expressions
  return ""
mkCode unknownCode = error $ "Could not convert code:" ++ show unknownCode

kernelTemplate :: KernelName -> [GPUBuffer] -> [GPUBuffer] -> [String] -> String
kernelTemplate name buffers buffersout code =
  mkKernelShell name buffers $
    " int int_index = get_global_id(0);\n"
      ++ intercalate "\n" (zipWith (gpuBufferAssign "int_index") buffersout code)

mkKernelShell :: KernelName -> [GPUBuffer] -> String -> String
mkKernelShell (KernelName name) args code = "void kernel " ++ name ++ "(" ++ argsStr ++ ")" ++ "{ \n" ++ code ++ " \n};"
  where
    argsStr = intercalate ", " (map gpuBufferDecl args)

gpuBufferDecl gpub@(GPUBuffer _ size) =
  "global int " ++ gpuBufferArgName gpub ++ "[" ++ show size ++ "]"

varVarname :: Exp -> String
varVarname (Var name True) = "int_" ++ name
varVarname (Var name False) = "arg_" ++ name

gpuBufferVar (GPUBuffer (GPUBufferName name) _) = Var ("array" ++ show name) True

gpuBufferArgName b@(GPUBuffer _ _) = varVarname $ gpuBufferVar b

gpuBufferAssign :: String -> GPUBuffer -> String -> String
gpuBufferAssign index buffer value = gpuBufferArgName buffer ++ "[" ++ index ++ "] = " ++ value ++ ";"

-- Add brackets
mkCodeB :: Exp -> SCode String String
mkCodeB x = do
  v <- mkCode x
  return $ "(" ++ v ++ ")"
