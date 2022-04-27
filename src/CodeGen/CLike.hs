module CodeGen.CLike where

import Code.Definitions
import Code.SCode
import Data.List
import Language.GaiwanDefs

mkBinOp a b op = do
  ka <- mkCodeB a
  kb <- mkCodeB b
  return $ ka ++ op ++ kb

mkCode :: BExp -> SCode String String
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
mkCode (Int num) = return $ show num
mkCode (Var name True) = return $ "int_" ++ name
mkCode (Var name False) = return $ "arg_" ++ name
mkCode (Negate x) = ("-" ++) <$> mkCodeB x
mkCode (ArrayGet x idx) = do
  kx <- mkCodeB x
  kidx <- mkCodeB idx
  return $ kx ++ "[" ++ kidx ++ "]"
mkCode (GPUBufferGet buffer idx) = mkCode (ArrayGet (gpuBufferVar buffer) idx)
mkCode unknownCode = error $ "Could not convert code:" ++ show unknownCode

kernelTemplate :: KernelName -> [ReservedBuffer] -> [ReservedBuffer] -> [String] -> String
kernelTemplate name buffers buffersout code =
  mkKernelShell name (buffers ++ buffersout) $
    " int int_index = get_global_id(0);\n"
      ++ intercalate "\n" (zipWith (gpuBufferAssign "int_index") buffersout code)

mkKernelShell :: KernelName -> [ReservedBuffer] -> String -> String
mkKernelShell (KernelName name) args code = "void kernel " ++ name ++ "(" ++ argsStr ++ ")" ++ "{ \n" ++ code ++ " \n};"
  where
    argsStr = intercalate ", " (map gpuBufferDecl args)

-- TODO: size is wrong here, but we do not know it yet, maybe use defines?
gpuBufferDecl gpub@(ReservedBuffer _ (GaiwanBuf (GaiwanBufSize id a b) GaiwanInt)) =
  "global int " ++ gpuBufferArgName gpub ++ "[LEN_" ++ show id ++ "_" ++ show a ++ "_" ++ show b ++ "]"

gpuBufferVar (ReservedBuffer (GPUBufferName name) _) = Var ("array" ++ show name) True

-- todo
gpuBufferArgName b@(ReservedBuffer (GPUBufferName name) _) = "int_array" ++ (show name)

gpuBufferAssign :: String -> ReservedBuffer -> String -> String
gpuBufferAssign index buffer value = gpuBufferArgName buffer ++ "[" ++ index ++ "] = " ++ value ++ ";"

-- Add brackets
mkCodeB :: BExp -> SCode String String
mkCodeB x = do
  v <- mkCode x
  return $ "(" ++ v ++ ")"
