-- for stmt
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Language.GaiwanDefs
  ( Program (..),
    Stmt (..),
    StmtType (..),
    GStmtType (..),
    Exp (..),
    TypedStmt (..),
    subst,
    substMult,
    substGPUBuffers,
    stmt,
    stmtName,
    simplifyExp,
    simpleSubstMult,
    simpleSubst,
    toTypedSmt,
    mergeT,
  )
where

import Code.Definitions
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.Maybe

data Program
  = Prog [Stmt] Exp
  deriving (Show, Eq)

data GStmtType a
  = GaiwanInt -- TODO: float
  | GaiwanTuple [GStmtType a]
  | GaiwanArrow (GStmtType a) (GStmtType a)
  | GaiwanBuf Exp (GStmtType a)
  | TVar a
  deriving (Show, Eq)

type StmtType = GStmtType String

data TypedStmt
  = TMapper StmtType String [String] Exp
  | TShaper StmtType String [String] Exp
  | TReducer StmtType String [String] Exp Exp
  | TAbstraction StmtType String [String] [TypedStmt]
  deriving (Show, Eq)

typedStmt (TMapper t _ _ _) = t
typedStmt (TShaper t _ _ _) = t
typedStmt (TReducer t _ _ _ _) = t
typedStmt (TAbstraction t _ _ _) = t

data Stmt
  = Mapper (Maybe StmtType) String [(String, Maybe StmtType)] Exp
  | Shaper (Maybe StmtType) String [(String, Maybe StmtType)] Exp
  | Reducer (Maybe StmtType) String [(String, Maybe StmtType)] Exp Exp
  | Abstraction (Maybe StmtType) String [(String, Maybe StmtType)] [Stmt]
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
  | Select Exp Int
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
    _simplifyExp (Times (Int 1) x) = Just x
    _simplifyExp (Times x (Int 1)) = Just x
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

maybeGaiwanInt :: Maybe StmtType -> Bool
maybeGaiwanInt indexType = fromMaybe GaiwanInt indexType == GaiwanInt

-- >>> toTypedSmt (Mapper Nothing "lil" [("i",Nothing),("v", Just GaiwanInt)] (Var "v" False))
-- Right (TMapper (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt)) "lil" ["i","v"] (Var "v" False))

toTypedSmt :: Stmt -> Either String TypedStmt
toTypedSmt = toTypedSmtEnv []

fuckThisShit :: [StmtType] -> Either String StmtType
fuckThisShit [a, b] = mergeT a b
fuckThisShit [a] = Right a
fuckThisShit (a : ar) = fuckThisShit ar >>= mergeT a
fuckThisShit [] = Left "empty merge"

toTypedSmtEnv :: [(String, StmtType)] -> Stmt -> Either String TypedStmt
toTypedSmtEnv env (Abstraction outType name args []) = Left "Cannot make abstraction without content"
toTypedSmtEnv env (Abstraction outType name args parts) = do
  things <- maybe (Left "all the arguments of an abstraction need to be set") Right $ mapM liftMaybeTuple args
  partType <- mapM (toTypedSmtEnv $ things ++ env) parts
  typedParts <- fuckThisShit $ map typedStmt partType
  outT <- doRightCase outType typedParts
  return $
    TAbstraction outT name (map fst args) partType
toTypedSmtEnv env (Mapper outType name args@[(indexArg, indexType), (dataVarName, Just dataVarType)] body)
  | maybeGaiwanInt indexType = do
    retType <- typeWithExpection outType ([(indexArg, GaiwanInt), (dataVarName, dataVarType)] ++ env) body
    return $
      TMapper
        ( GaiwanArrow
            (GaiwanBuf (Var "n" False) dataVarType)
            (GaiwanBuf (Var "n" False) retType)
        )
        name
        (map fst args)
        body
toTypedSmtEnv env (Shaper outType@(Just outTypeR@(GaiwanBuf _ elemType)) name args@((indexArg, indexType) : otherArgs) body)
  | maybeGaiwanInt indexType = do
    extraArgsBuf <- maybe (Left "all the arguments of a shaper must have a specified buffer type") Right $ mapM liftMaybeTuple otherArgs
    extraArgs <- mapM liftMaybeBuffElemType otherArgs
    typeWithExpection (Just elemType) (((indexArg, GaiwanInt) : extraArgsBuf) ++ env) body
    return $
      TShaper
        (foldr (GaiwanArrow . snd) outTypeR extraArgsBuf)
        name
        (map fst args)
        body
toTypedSmtEnv env (Reducer outType name args@[(indexArg, indexType), (accArg, accType), (dataArg, dataType@(Just dataTypeR))] initExp body)
  | maybeGaiwanInt indexType = do
    checkecAccType <- typeWithExpection accType env initExp
    outElemType <- bufferType1 outType
    checkedOutType <- typeWithExpectionAndJustArgs outElemType ([(indexArg, Just GaiwanInt), (accArg, Just checkecAccType), (dataArg, dataType)] ++ map (second Just) env) body
    return $
      TReducer
        (GaiwanArrow (GaiwanBuf (Var "n" False) dataTypeR) (GaiwanBuf (Int 1) checkedOutType))
        name
        (map fst args)
        initExp
        body

type TaggedStmtType = GStmtType (Int, String)

-- | merge two arrow types like function composition (and adjust buffer sizes if needed)
mergeT :: StmtType -> StmtType -> Either String StmtType
mergeT t1 t2 = unrename <$> joinT (applym theC ty1) (applym theC ty2)
  where
    ty1 = rename 1 t1
    ty2 = rename 2 t2

    toMatch1 = lstT ty1
    toMatch2 = fstT ty2

    joinT :: (Eq a, Show a) => GStmtType a -> GStmtType a -> Either String (GStmtType a)
    joinT = joinT1 []

    joinT1 :: (Eq a, Show a) => [GStmtType a] -> GStmtType a -> GStmtType a -> Either String (GStmtType a)
    joinT1 l (GaiwanArrow a@(GaiwanBuf size1 ta) b) c = joinT1 (a : l) b c
    --joinT1 l a (GaiwanArrow b c) | a == b = -- it just works?
    joinT1 l a@(GaiwanBuf size1 ta) (GaiwanArrow b@(GaiwanBuf size2 tb) c@(GaiwanBuf size3 tc)) | ta == tb = fixN l a b c
    joinT1 l a@(GaiwanBuf size1 ta) (GaiwanArrow b c@(GaiwanArrow _ _)) = error $ "join with deep arry:" ++ show c
    joinT1 l a b = error $ "someting went wrong joining" ++ show (a, b)

    lstT (GaiwanArrow _ b) = lstT b
    lstT a = a

    fstT (GaiwanArrow b _) = b

    theC = constraints toMatch1 toMatch2
    constraints :: TaggedStmtType -> TaggedStmtType -> [((Int, String), TaggedStmtType)]
    constraints (TVar a) (TVar b) | a == b = []
    constraints (TVar a) b = [(a, b)]
    constraints a (TVar b) = [(b, a)]
    constraints GaiwanInt GaiwanInt = []
    constraints (GaiwanTuple a) (GaiwanTuple b) = listConstraints a b
    constraints (GaiwanArrow a1 a2) (GaiwanArrow b1 b2) = listConstraints [a1, a2] [b1, b2]
    constraints (GaiwanBuf _ a) (GaiwanBuf _ b) = constraints a b

    listConstraints :: [TaggedStmtType] -> [TaggedStmtType] -> [((Int, String), TaggedStmtType)]
    listConstraints (a : ar) (b : bs) = let prev = constraints a b in prev ++ listConstraints (map (applym prev) ar) (map (applym prev) bs)
    listConstraints [] [] = []

    applym :: [((Int, String), TaggedStmtType)] -> TaggedStmtType -> TaggedStmtType
    applym m = apply $ \name -> fromMaybe (TVar name) (lookup name m)

    rename :: Int -> StmtType -> TaggedStmtType
    rename v = apply $ \name -> TVar (v, name)

    apply :: (a -> GStmtType b) -> GStmtType a -> GStmtType b
    apply f (TVar a) = f a
    apply f GaiwanInt = GaiwanInt
    apply f (GaiwanTuple a) = GaiwanTuple $ map (apply f) a
    apply f (GaiwanArrow a1 a2) = GaiwanArrow (apply f a1) (apply f a2)
    apply f (GaiwanBuf n a) = GaiwanBuf n $ apply f a

    unrename :: TaggedStmtType -> StmtType
    unrename = apply $ \name -> TVar $ show name

fixN :: [GStmtType a] -> GStmtType a -> GStmtType a -> GStmtType a -> Either String (GStmtType a)
fixN l (GaiwanBuf size2 _) (GaiwanBuf size3 _) (GaiwanBuf size4 resType) = do
  solvedCount <- solveTCnt (map norm counts) (norm size2) (norm size3) (norm size4)
  Right $ uncurry aaaaa solvedCount
  where
    norm (Times (Int a2) (Var name2 False)) = Plus (Times (Int a2) (Var name2 False)) (Int 0)
    norm (Times (Var name2 False) (Int a2)) = Plus (Times (Int a2) (Var name2 False)) (Int 0)
    norm (Var name2 False) = Plus (Times (Int 1) (Var name2 False)) (Int 0)
    norm (Plus (Var name2 False) (Int b)) = Plus (Times (Int 1) (Var name2 False)) (Int b)
    norm x@(Plus (Times (Int a) (Var name2 False)) (Int b)) = x
    norm idk = error $ show idk -- TODO
    counts = map (\(GaiwanBuf size _) -> size) l

    aaaaa argSizes endsize = bbb l argSizes (GaiwanBuf (simplifyExp endsize) resType)

    bbb [] [] c = c
    bbb ((GaiwanBuf _ t) : lr) (s : sr) acc = bbb lr sr $ GaiwanArrow (GaiwanBuf (simplifyExp s) t) acc

solveTCnt :: [Exp] -> Exp -> Exp -> Exp -> Either String ([Exp], Exp)
solveTCnt
  ll
  (Plus (Times (Int a2) (Var name2 False)) (Int b2))
  (Plus (Times (Int a3) (Var name3 False)) (Int b3))
  (Plus (Times (Int a4) (Var name4 False)) (Int b4))
    | name3 == name4 = do
      v <- case a2 `mod` a3 of
        0 -> case (b3 - b2) `mod` a3 of
          0 -> Right 0
          _ -> Left "dddddddd"
        _ -> case divMod (b3 - b2) a2 of
          (r1, 0) -> Right $ mod r1 a3
          _ -> Left $ "The constant differences in buffersizes are not unifiable " ++ show ((b3 - b2), a2)
      Right
        ( map
            ( \(Plus (Times (Int a1) (Var _ False)) (Int b1)) -> Plus (Times (Int $ a1 * u) n) (Int $ b1 + a1 * v)
            )
            ll,
          Plus (Times (Int $ a4 * div a2 (gcd a2 a3)) n) (Int $ b4 + a4 * div (a2 * v + b2 - b3) a3)
        )
    where
      a2Inva3 = if a3 == 1 then 1 else head $ filter (\x -> (x * a2) `mod` a3 == 1) [1 .. a3]
      n = Var name2 False
      u = div a3 (gcd a2 a3)

liftEitherTuple :: (a, Either b c) -> Either b (a, c)
liftEitherTuple (a, Right x) = Right (a, x)
liftEitherTuple (a, Left x) = Left x

liftMaybeTuple :: (a, Maybe b) -> Maybe (a, b)
liftMaybeTuple (a, Just x) = Just (a, x)
liftMaybeTuple (a, Nothing) = Nothing

bufferType1 :: Maybe StmtType -> Either String (Maybe StmtType)
bufferType1 (Just (GaiwanBuf (Int 1) b)) = Right $ Just b
bufferType1 (Just GaiwanBuf {}) = Left "non buffer type with length one for out of reducer"
bufferType1 (Just _) = Left "non buffer type"
bufferType1 Nothing = Right Nothing

bufferType :: Maybe StmtType -> Either String StmtType
bufferType (Just (GaiwanBuf _ b)) = Right b
bufferType (Just _) = Left "non buffer type"
bufferType Nothing = Left "no type given"

-- | Get the type of the elements of a buffer argument
liftMaybeBuffElemType :: (String, Maybe StmtType) -> Either String (String, StmtType)
liftMaybeBuffElemType (a, b) = (a,) <$> bufferType b

-- >>> mapM liftEitherTuple [(5, Right 1), (6, Right 2)] :: Either String [(Integer, Integer)]
-- Right [(5,1),(6,2)]
-- >>> mapM liftEitherTuple [(5, Right 1), (6, Left "nope")] :: Either String [(Integer, Integer)]
-- Left "nope"

-- >>> mapM liftMaybeTuple [(5, Just 1), (6, Nothing)] :: Maybe [(Integer, Integer)]
-- Nothing
-- >>> mapM liftMaybeTuple [(5, Just 1), (6, Just 4)] :: Maybe [(Integer, Integer)]
-- Just [(5,1),(6,4)]

typeWithExpectionAndJustArgs :: Maybe StmtType -> [(String, Maybe StmtType)] -> Exp -> Either String StmtType
typeWithExpectionAndJustArgs x args exp = maybe (Left "failed to lift maybe tuple") b (mapM liftMaybeTuple args)
  where
    b :: [(String, StmtType)] -> Either String StmtType
    b actualArgs = typeWithExpection x actualArgs exp

typeWithExpection :: Maybe StmtType -> [(String, StmtType)] -> Exp -> Either String StmtType
typeWithExpection x args exp = either Left (doRightCase x) (typeOfBody args exp)

doRightCase :: Maybe StmtType -> StmtType -> Either String StmtType
doRightCase Nothing b = Right b
doRightCase (Just a) b | a == b = Right b
doRightCase (Just a) b = Left $ "Expected outtype does not match " ++ show a ++ " but got " ++ show b

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
typeOfBody env (Select t index) = case typeOfBody env t of
  Right (GaiwanTuple types) | index >= 0 && index < length types -> Right $ types !! index
  Right (GaiwanTuple types) -> Left $ "Tuple index (" ++ show index ++ ") out of range"
  Right t -> Left $ "Can only select from Tuple, not from " ++ show t
  Left a -> Left a
typeOfBody env (Tuple exps) = GaiwanTuple <$> mapM (typeOfBody env) exps
typeOfBody env var@Var {} = typeOfVar env var
typeOfBody env (Negate e) = case typeOfBody env e of
  Right GaiwanInt -> Right GaiwanInt
  _ -> Left "Cannot negate"
typeOfBody env (If cond tBranch fBrach) = case mapM (typeOfBody env) [cond, tBranch, fBrach] of
  Right [tC, tT, fT] | tC == GaiwanInt && tT == fT -> Right tT
  Right _ -> Left "Types of if did not match"
  Left x -> Left x
typeOfBody env PipedExp {} = Left "Cannot type a piped expression in a body"
typeOfBody env App {} = Left "I don't know this applicaion"
typeOfBody env GPUBufferGet {} = Left "Cannot use GPUBufferget in body"
typeOfBody env Loop {} = Left "Cannot use loop in body"

typeOfVar :: [(String, StmtType)] -> Exp -> Either String StmtType
typeOfVar _ (Var name True) = error "not implemented"
typeOfVar env (Var name False) = maybe (Left $ name ++ " not in env") Right $ lookup name env

typeOfMathBinop env a b = case mapM (typeOfBody env) [a, b] of
  Right [GaiwanInt, GaiwanInt] -> Right GaiwanInt
  Left e -> Left e
  _ -> Left "failed to type binop"

typeOfArrayAccess :: [(String, StmtType)] -> Exp -> Exp -> Either String StmtType
typeOfArrayAccess env array index = case mapM (typeOfBody env) [array, index] of
  Right [GaiwanBuf _ ty, GaiwanInt] -> Right ty
  Right [GaiwanBuf _ ty, _] -> Left "failed to type array access: index is not an int"
  Right [at, GaiwanInt] -> Left $ "failed to type array access:" ++ show array ++ " was not an array but " ++ show at
  Right _ -> Left "failed to type array access: idk"
  Left e -> Left e