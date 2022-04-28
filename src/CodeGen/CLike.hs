module CodeGen.CLike where

import Code.Definitions
import Code.SCode
import Control.Monad.State (StateT, get, put, runStateT)
import Data.List
import Language.GaiwanDefs
import Debug.Trace

mkBinOp :: BExp -> BExp -> String -> CSCode String
mkBinOp a b op = do
  ka <- mkCodeB a
  kb <- mkCodeB b
  return $ ka ++ op ++ kb

mkCode :: BExp -> SCode String (String, String)
mkCode e | trace ("\nKKKKKKKKKKKKKKKKK: " ++ (take 10000 $ show e)) False = undefined
mkCode e = do
    (code, CAdmin _ _ bounds) <- runStateT (mkCCode e) (CAdmin 0 [] [])
    let letBindCode = map f bounds
    return ((intercalate "\n" letBindCode), code)

f :: (Int, String) -> String
f (id, impl) = "int let_"++(show id) ++ "= "++impl++";"

data CAdmin = CAdmin Int [(String, Int)] [(Int, String)]

type CSCode = StateT CAdmin (SCode String)

mkCCode :: BExp -> CSCode String
mkCCode e | trace ("mkCCode: " ++ (take 100 $ show e)) False = undefined
mkCCode (Let name value exp) = do
  valueStr <- mkCCode value
  CAdmin letCnt mapping doneMapping <- get
  put $ CAdmin (letCnt + 1) ((name, letCnt) : mapping) doneMapping
  expStr <- mkCCode exp
  CAdmin letCnt' mapping' doneMapping' <- get
  put $ CAdmin letCnt' mapping ((letCnt, valueStr) : doneMapping')
  return expStr
mkCCode (Plus a b) = mkBinOp a b "+"
mkCCode (Minus a b) = mkBinOp a b "-"
mkCCode (Times a b) = mkBinOp a b "*"
mkCCode (Div a b) = mkBinOp a b "/"
mkCCode (Modulo a b) = mkBinOp a b "%"
mkCCode (Pow (Int 2) b) = mkBinOp (Int 1) b "<<" -- TODO: what hapens if b is too large?
mkCCode (Pow a b) = (\args -> "pow(" ++ args ++ ")") <$> mkBinOp a b ","
mkCCode (IsEq a b) = mkBinOp a b "=="
mkCCode (IsGreater a b) = mkBinOp a b "<"
mkCCode (If cond texp fexp) = do
  condStr <- mkCodeB cond
  restultStr <- mkBinOp texp fexp ":"
  return $ condStr ++ "?" ++ restultStr
mkCCode (Int num) = return $ show num
mkCCode (Var name True) = return $ "int_" ++ name
mkCCode (Var name False) = do
  CAdmin letCnt mapping doneMapping <- get
  return $ case lookup name mapping of
    Nothing -> "arg_" ++ name
    (Just n) -> "let_" ++ show n
mkCCode (Negate x) = ("-" ++) <$> mkCodeB x
mkCCode (ArrayGet x idx) = do
  kx <- mkCodeB x
  kidx <- mkCodeB idx
  return $ kx ++ "[" ++ kidx ++ "]"
mkCCode (GPUBufferGet buffer idx) = mkCCode (ArrayGet (gpuBufferVar buffer) idx)
mkCCode unknownCode = error $ "Could not convert code:" ++ (take 30 $ show unknownCode) ++ "..."

kernelTemplate :: KernelName -> [ReservedBuffer] -> [ReservedBuffer] -> [(String,String)] -> String
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

gpuBufferAssign :: String -> ReservedBuffer -> (String,String) -> String
gpuBufferAssign index buffer (letBinds,value) = "{"++letBinds++"\n"++gpuBufferArgName buffer ++ "[" ++ index ++ "] = " ++ value ++ ";}"

-- Add brackets
mkCodeB :: BExp -> CSCode String
mkCodeB x = do
  v <- mkCCode x
  return $ "(" ++ v ++ ")"
