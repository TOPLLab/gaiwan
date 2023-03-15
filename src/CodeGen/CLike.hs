module CodeGen.CLike where

import Code.Definitions
import Code.SCode
import Control.Monad.State (StateT, get, put, runStateT)
import Data.List
import Debug.Trace
import Language.GaiwanDefs

mkBinOp :: BExp -> BExp -> String -> CSCode String
mkBinOp a b op = do
  ka <- mkCodeB a
  kb <- mkCodeB b
  return $ ka ++ op ++ kb

mkCode :: BExp -> SCode String (String, String)
-- mkCode e | trace ("\nKKKKKKKKKKKKKKKKK: " ++ (take 10000 $ show e)) False = undefined
mkCode e = do
  (code, CAdmin _ _ bounds) <- runStateT (mkCCode e) (CAdmin 0 [] [])
  let letBindCode = map f bounds
  return (intercalate "\n" letBindCode, code)

f :: (Int, String) -> String
f (id, impl) = "int let_" ++ show id ++ "= " ++ impl ++ ";"

data CAdmin = CAdmin Int [(String, Int)] [(Int, String)]

type CSCode = StateT CAdmin (SCode String)

mkCCode :: BExp -> CSCode String
-- mkCCode e | trace ("mkCCode: " ++ (take 100 $ show e)) False = undefined
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
mkCCode unknownCode = error $ "Could not convert code:" ++ take 30 (show unknownCode) ++ "..."

kernelTemplate :: KernelName -> [ReservedBuffer] -> [ReservedBuffer] -> [(String, String)] -> String
kernelTemplate name buffers buffersout code =
  mkKernelShell name (buffers ++ buffersout) $
    " int int_i = get_global_id(0);\n"
      ++ intercalate "\n" (zipWith (gpuBufferAssign "int_i") buffersout code)

-- Reducer in a dumb way all on one core...
reducerKernelTemplate :: KernelName -> [ReservedBuffer] -> ReservedBuffer -> (String, String) -> String
reducerKernelTemplate name [buffer@(ReservedBuffer _ gb@(GaiwanBuf _ GaiwanInt))] bufferout code =
  mkKernelShell name [buffer, bufferout] $
    "if(0==get_global_id(0)){\n"
      ++ " int int_acc = 0;\n" -- TODO use actual init acc
      ++ "for(int int_i = 0 ;int_i < "
      ++ lenDefineName gb
      ++ ";int_i++){\n"
      ++ variableAssign "int_acc" code -- update accumulator
      ++ "}\n"
      ++ gpuBufferAssign "0" bufferout ("", "int_acc") -- write to pos 0
      ++ "\n}\n"
reducerKernelTemplate _ _ _ _ = error "Illegal nubmer of buffers"

-- TODO: use bufferout to type accumulator and initial value
assocReducerKernelTemplate ::
  KernelName ->
  KernelName ->
  [ReservedBuffer] ->
  ReservedBuffer ->
  (String, String) -> -- code
  (String, String) -> -- combination code
  String
assocReducerKernelTemplate
  name1
  name2
  buffers@((ReservedBuffer _ gb):_)
  bufferout@(ReservedBuffer _ goutb@(GaiwanBuf _ GaiwanInt))
  codeValue
  codeAcc = firstStage ++ secondStage
    where
      accType = "int"
      intermediateVar = "intermediate"
      intermediateArg = "global " ++ accType ++ "* " ++ intermediateVar
      intermediateLenArg = "global uint* " ++ intermediateLenVar
      intermediateLenVar = "intermediateLEN"
      -- First stage: 2n->n
      firstStage =
        mkCustomKernelShell name1 ((map gpuBufferDecl buffers) ++ [intermediateLenArg, intermediateArg]) $
          " int int_i = 2*get_global_id(0);\n"
            ++ accType
            ++ " int_acc = 0;\n" -- TODO use actual init acc
            ++ variableAssign "int_acc" codeValue -- update accumulator
            ++ "\nint_i++;\n" -- == 2*get_global_id(0) + 1
            ++ "if(int_i < "
            ++ lenDefineName gb
            ++ "){"
            ++ variableAssign "int_acc" codeValue -- update accumulator
            ++ "\n}\n"
            ++ variableAssign (intermediateVar ++ "[get_global_id(0)]") ("", "int_acc") -- write to pos int_i
      secondStage =
        mkCustomKernelShell name2 [gpuBufferDecl bufferout, "global uint* stepsizePtr", intermediateLenArg, intermediateArg] $
          "int stepsize = *stepsizePtr;"
            ++ "int int_i = (2*get_global_id(0))*stepsize;\n"
            ++ "if(int_i + stepsize < *"
            ++ intermediateLenVar
            ++ "){\n"
            ++ accType
            ++ " int_v1 = "
            ++ intermediateVar
            ++ "[int_i];\n"
            ++ accType
            ++ " int_v2 = "
            ++ intermediateVar
            ++ "[int_i+stepsize];\n"
            ++ accType
            ++ " int_acc; "
            ++ variableAssign "int_acc" codeAcc -- update accumulator
            ++ variableAssign "intermediate[int_i]" ("", "int_acc") -- update accumulator
            ++ "\nif(int_i==0){"
            ++ gpuBufferAssign "int_i" bufferout ("", "int_acc") -- write to pos int_i
            ++ "}\n"
            ++ "}\n"
assocReducerKernelTemplate _ _ _ _ _ _ = error "TODO"

mkKernelShell :: KernelName -> [ReservedBuffer] -> String -> String
mkKernelShell name args = mkCustomKernelShell name (map gpuBufferDecl args)

mkCustomKernelShell :: KernelName -> [String] -> String -> String
mkCustomKernelShell (KernelName name) args code = "void kernel " ++ name ++ "(" ++ argsStr ++ ")" ++ "{ \n" ++ code ++ " \n};\n"
  where
    argsStr = intercalate ", " args

-- TODO: size is wrong here, but we do not know it yet, maybe use defines?
gpuBufferDecl gpub@(ReservedBuffer _ buffer@(GaiwanBuf (GaiwanBufSize id a b) _)) =
  "global int " ++ gpuBufferArgName gpub ++ "[" ++ lenDefineName buffer ++ "]"

lenDefineName (GaiwanBuf (GaiwanBufSize id a b) _) = "LEN_" ++ show id ++ "_" ++ show a ++ "_" ++ show b

gpuBufferVar (ReservedBuffer (GPUBufferName name) _) = Var ("array" ++ show name) True

-- todo
gpuBufferArgName b@(ReservedBuffer (GPUBufferName name) _) = "int_array" ++ show name

gpuBufferAssign :: String -> ReservedBuffer -> (String, String) -> String
gpuBufferAssign index buffer = variableAssign (gpuBufferArgName buffer ++ "[" ++ index ++ "]")

variableAssign name ("", value) = name ++ " = " ++ value ++ ";\n"
variableAssign name (letBinds, value) = "{" ++ letBinds ++ "\n" ++ name ++ " = " ++ value ++ ";\n};"

-- Add brackets
mkCodeB :: BExp -> CSCode String
mkCodeB x = do
  v <- mkCCode x
  return $ "(" ++ v ++ ")"
