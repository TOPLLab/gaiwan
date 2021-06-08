-- for stmt
{-# LANGUAGE RankNTypes #-}

module Language.GaiwanDefs
  ( Program (..),
    Stmt (..),
    StmtType (..),
    Exp (..),
    subst,
    substMult,
    substGPUBuffers,
    stmt,
    stmtName,
    simplifyExp,
    simpleSubstMult,
    simpleSubst,
  )
where

import Code.Definitions
import Data.Either
import Data.Maybe

data Program
  = Prog [Stmt] Exp
  deriving (Show, Eq)

data StmtType
  = GaiwanInt -- TODO: float
  | GaiwanTuple [StmtType]
  | GaiwanArrow StmtType StmtType
  | GaiwanBuf Exp StmtType
  | TVar String
  deriving (Show, Eq)

data Stmt
  = Mapper (Maybe StmtType) String [(String, Maybe StmtType)] Exp
  | Shaper (Maybe StmtType) String [(String, Maybe StmtType)] Exp
  | Reducer (Maybe StmtType) String [(String, Maybe StmtType)] Exp Exp
  deriving (Show, Eq)

stmt :: forall t. Stmt -> (String -> [(String, Maybe StmtType)] -> Exp -> t) -> t
stmt (Shaper _ a b c) f = f a b c
stmt (Mapper _ a b c) f = f a b c

stmtName :: Stmt -> String
stmtName x = stmt x (\name b c -> name)

data Exp
  = Let String Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | App String Bool [Exp]
  | Modulo Exp Exp
  | Times Exp Exp
  | Pow Exp Exp
  | Div Exp Exp
  | Int Int
  | Tuple [Exp]
  | Var String Bool
  | Negate Exp
  | PipedExp [Exp]
  | ArrayGet Exp Exp
  | GPUBufferGet GPUBuffer Exp -- Not expressable in syntax
  | Loop Exp String [Exp]
  | If Exp Exp Exp
  | IsEq Exp Exp
  | IsGreater Exp Exp
  deriving (Show, Eq)

simpleSubst :: Exp -> Exp -> Exp -> Exp
simpleSubst from to c = simplifyExp $ subst from to c

simpleSubstMult :: [(Exp, Exp)] -> Exp -> Exp
simpleSubstMult mapping to = simplifyExp $ substMult mapping to

-- Substiute a for b in c
subst :: Exp -> Exp -> Exp -> Exp
subst from to = substMult [(from, to)]

-- Substitute
substMult :: [(Exp, Exp)] -> Exp -> Exp
substMult [] c = c
substMult kv c = mapExp (_subst kv) c
  where
    -- loop: remove var
    _subst kv c@(Loop cntExp varname exps) =
      Just $
        Loop
          (simplifyExp $ substMult kv cntExp)
          varname
          (map (simpleSubstMult $ delKey (Var varname False) kv) exps)
    -- otherwise, lookup in kv and replace
    _subst kv c = lookup c kv

substGPUBuffers :: [(GPUBuffer, GPUBuffer)] -> Exp -> Exp
substGPUBuffers [] c = c
substGPUBuffers kv c = mapExp (_subst kv) c
  where
    -- loop: remove var
    _subst kv c@(GPUBufferGet buf exp) = do
      otherBuf <- lookup buf kv
      return $ GPUBufferGet otherBuf (substGPUBuffers kv exp)
    _subst kv c = Nothing

-- | Simplify till fixed point
simplifyExp :: Exp -> Exp
simplifyExp e =
  let rec = mapExp _simplifyExp e
   in if rec == e then e else simplifyExp rec
  where
    _simplifyExp (Plus (Int a) (Int b)) = Just $ Int $ a + b
    _simplifyExp (Plus (Int 0) b) = Just $ simplifyExp b
    _simplifyExp (Plus a (Int 0)) = Just $ simplifyExp a
    _simplifyExp (Minus (Int a) (Int b)) = Just $ Int $ a - b
    _simplifyExp (Times (Int a) (Int b)) = Just $ Int $ a * b
    _simplifyExp (Times (Int 0) _) = Just $ Int 0
    _simplifyExp (Times _ (Int 0)) = Just $ Int 0
    _simplifyExp (Div (Int a) (Int b)) = Just $ Int $ div a b
    _simplifyExp (Div a (Int 1)) = Just a
    _simplifyExp (Modulo _ (Int 1)) = Just $ Int 0
    _simplifyExp (Modulo (Int a) (Int b)) = Just $ Int $ mod a b
    _simplifyExp (Pow (Int a) (Int b)) = Just $ Int $ a ^ b
    _simplifyExp e = Nothing

delKey :: Exp -> [(Exp, Exp)] -> [(Exp, Exp)]
delKey key = filter filterFun
  where
    filterFun (k, _) | k == key = False
    filterFun (k, _) = True

-- Recursively perform an function on an expression.
-- If the given function return
--    - None: The call is retried on the components
--    - Just x: mapExp returns x (the functions should take care of recursion)
mapExp :: (Exp -> Maybe Exp) -> Exp -> Exp
mapExp fOrig e = fromMaybe (_mapExp e) (fOrig e)
  where
    f = mapExp fOrig
    _mapExp :: Exp -> Exp
    _mapExp Let {} = error "not supported"
    _mapExp e@(Plus a b) = Plus (f a) (f b)
    _mapExp e@(Minus a b) = Minus (f a) (f b)
    _mapExp e@(App name builtin exps) = App name builtin (map f exps)
    _mapExp e@(Modulo a b) = Modulo (f a) (f b)
    _mapExp e@(Times a b) = Times (f a) (f b)
    _mapExp e@(Pow a b) = Pow (f a) (f b)
    _mapExp e@(Div a b) = Div (f a) (f b)
    _mapExp e@(Negate a) = Negate (f a)
    _mapExp e@(PipedExp steps) = PipedExp (map f steps)
    _mapExp e@(ArrayGet a b) = ArrayGet (f a) (f b)
    _mapExp e@(Loop cnt name exps) = Loop (f cnt) name (map f exps)
    _mapExp e@(If a b c) = If (f a) (f b) (f c)
    _mapExp e@(IsEq a b) = IsEq (f a) (f b)
    _mapExp e@(IsGreater a b) = IsGreater (f a) (f b)
    _mapExp x = x -- Int, Var

data TypedStmt
  = TMapper StmtType String [String] Exp
  | TShaper StmtType String [String] Exp
  | TReducer StmtType String [String] Exp Exp
  deriving (Show, Eq)


maybeGaiwanInt :: Maybe StmtType -> Bool
maybeGaiwanInt indexType = fromMaybe GaiwanInt indexType == GaiwanInt

toTypedSmt :: Stmt -> Either String TypedStmt
toTypedSmt (Mapper outType name args@[(indexArg, indexType), (dataVarName, Just dataVarType)] body)
  | maybeGaiwanInt indexType =
    ( \x ->
        TMapper
          ( GaiwanArrow
              (GaiwanBuf (Var "n" False) dataVarType)
              (GaiwanBuf (Var "n" False) x)
          )
          name
          (map fst args)
          body
    )
      <$> typeWithExpection outType [(indexArg, GaiwanInt), (dataVarName, dataVarType)] body
toTypedSmt (Shaper outType@(Just outTypeR) name args@((indexArg, indexType) : otherArgs) body)
  | maybeGaiwanInt indexType =
    ( \x ->
        TMapper
          (foldr (GaiwanArrow . snd) outTypeR realArgs)
          name
          (map fst args)
          body
    )
      <$> typeWithExpectionAndJustArgs
        outType
        ( (indexArg, Just GaiwanInt) :otherArgs )
        body
        where realArgs = zipWith   (\(argName,argType) num -> (argName,fromMaybe (TVar $ "avar" ++ show num) argType)) otherArgs  [1 ..]


liftEitherTuple :: (a, Either b c) -> Either b (a,c)
liftEitherTuple (a, Right x) = Right (a, x)
liftEitherTuple (a, Left x) = Left x

liftMaybeTuple :: (a, Maybe b) -> Maybe (a,b)
liftMaybeTuple (a, Just x) = Just (a, x)
liftMaybeTuple (a, nothing) = Nothing


-- >>> mapM liftEitherTuple [(5, Right 1), (6, Right 2)] :: Either String [(Integer, Integer)]
-- Right [(5,1),(6,2)]
-- >>> mapM liftEitherTuple [(5, Right 1), (6, Left "nope")] :: Either String [(Integer, Integer)]
-- Left "nope"

-- >>> mapM liftMaybeTuple [(5, Just 1), (6, Nothing)] :: Maybe [(Integer, Integer)]
-- Nothing
-- >>> mapM liftMaybeTuple [(5, Just 1), (6, Just 4)] :: Maybe [(Integer, Integer)]
-- Just [(5,1),(6,4)]


typeWithExpectionAndJustArgs :: Maybe StmtType -> [(String, Maybe StmtType)] -> Exp -> Either String StmtType
typeWithExpectionAndJustArgs x args exp = maybe (Left "failed") b (mapM liftMaybeTuple args)
  where
    b :: [(String, StmtType)] ->  Either String StmtType
    b actualArgs = typeWithExpection x actualArgs exp

typeWithExpection :: Maybe StmtType -> [(String, StmtType)] -> Exp -> Either String StmtType
typeWithExpection x args exp = either Left (doRightCase x) (typeOfBody args exp)

doRightCase :: Maybe StmtType -> StmtType -> Either String StmtType
doRightCase Nothing b = Right b
doRightCase (Just a) b | a == b = Right b
doRightCase (Just a) b = Left "Guest wrong type"

typeOfBody :: [(String, StmtType)] -> Exp -> Either String StmtType
typeOfBody env Let {} = Left "let not yet supported"
typeOfBody env (Plus a b) = typeOfMathBinop env a b
typeOfBody env (Minus a b) = typeOfMathBinop env a b
typeOfBody env (Modulo a b) = typeOfMathBinop env a b
typeOfBody env (Times a b) = typeOfMathBinop env a b
typeOfBody env (Pow a b) = typeOfMathBinop env a b
typeOfBody env (Div a b) = typeOfMathBinop env a b
typeOfBody env (IsEq a b) = typeOfMathBinop env a b
typeOfBody env (IsGreater a b) = typeOfMathBinop env a b
typeOfBody env (ArrayGet a b) = typeOfArrayAccess env a b -- FIXME
typeOfBody env (Int _) = Right GaiwanInt
typeOfBody env (Tuple exps) =
  if length itemTypes == length exps
    then Right $ GaiwanTuple itemTypes
    else Left "failed to type if"
  where
    itemTypes = rights $ map (typeOfBody env) exps
typeOfBody env var@Var {} = typeOfVar env var
typeOfBody env (Negate e) = case typeOfBody env e of
  Right GaiwanInt -> Right GaiwanInt
  _ -> Left "Cannot negate"
typeOfBody env (If cond tBranch fBrach) = case mapM (typeOfBody env) [cond, tBranch, fBrach] of
  Right [tC, tT, fT] | (tC == GaiwanInt) &&  (tT == fT) -> Right tT
  Right _ -> Left "Types of if did not match"
  Left x -> Left x
typeOfBody env PipedExp {} = Left "Cannot type a piped expression in a body"
typeOfBody env App {} = Left "I don't know this applicaion"
typeOfBody env GPUBufferGet {} = Left "Cannot use GPUBufferget in body"
typeOfBody env Loop {} = Left "Cannot use loop in body"

typeOfVar :: [(String, StmtType)] -> Exp -> Either String StmtType
typeOfVar _ (Var name True) = error "not implemented"
typeOfVar env (Var name False) = maybe (Left "Name not in env") Right $ lookup name env

typeOfMathBinop env a b = case map (typeOfBody env) [a, b] of
  [Right GaiwanInt, Right GaiwanInt] -> Right GaiwanInt
  _ -> Left "failed to type binop"


typeOfArrayAccess :: [(String, StmtType)] -> Exp -> Exp -> Either String StmtType
typeOfArrayAccess env array index  = case mapM (typeOfBody env) [array, index] of
  Right [GaiwanBuf _ ty, GaiwanInt] -> Right ty
  _ -> Left "failed to type array access"
