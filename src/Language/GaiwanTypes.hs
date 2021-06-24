{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Language.GaiwanTypes
  ( Program (..),
    Stmt (..),
    StmtType (..),
    GStmtType (..),
    TypedStmt (..),
    stmtName,
    toTypedSmt,
    stmt,
    mergeT,
  )
where

import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Language.GaiwanDefs

data GStmtType a
  = GaiwanInt -- TODO: float
  | GaiwanTuple [GStmtType a]
  | GaiwanArrow (GStmtType a) (GStmtType a)
  | GaiwanBuf Exp (GStmtType a)
  | TVar a
  deriving (Show, Eq)

type StmtType = GStmtType String

type TaggedStmtType = GStmtType (Int, String)

type TypeingOut = Either String

data TypedStmt
  = TMapper StmtType String [String] Exp
  | TShaper StmtType String [String] Exp
  | TReducer StmtType String [String] Exp Exp
  | TAbstraction StmtType String [String] [TypedStmt]
  deriving (Show, Eq)

typedStmt :: TypedStmt -> StmtType
typedStmt (TMapper t _ _ _) = t
typedStmt (TShaper t _ _ _) = t
typedStmt (TReducer t _ _ _ _) = t
typedStmt (TAbstraction t _ _ _) = t

data Program
  = Prog [Stmt] Exp
  deriving (Show, Eq)

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

maybeGaiwanInt :: Maybe StmtType -> Bool
maybeGaiwanInt indexType = fromMaybe GaiwanInt indexType == GaiwanInt

toTypedSmt :: Stmt -> TypeingOut TypedStmt
toTypedSmt = toTypedSmtEnv []

toTypedSmtEnv :: [(String, StmtType)] -> Stmt -> TypeingOut TypedStmt
toTypedSmtEnv env (Abstraction outType name args []) = Left "Cannot make abstraction without content"
toTypedSmtEnv env (Abstraction outType name args parts) = do
  things <- checkSndJustList "all the arguments of an abstraction need to be set" args
  partType <- mapM (toTypedSmtEnv $ things ++ env) parts
  typedParts <- mergeTList $ map typedStmt partType
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
    extraArgsBuf <- checkSndJustList "all the arguments of a shaper must have a specified buffer type" otherArgs
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

mergeTList :: [StmtType] -> TypeingOut StmtType
mergeTList [a, b] = mergeT a b
mergeTList [a] = Right a
mergeTList (a : ar) = mergeTList ar >>= mergeT a
mergeTList [] = Left "empty merge"

-- | merge two arrow types like function composition (and adjust buffer sizes if needed)
mergeT :: StmtType -> StmtType -> TypeingOut StmtType
mergeT t1 t2 = unrename <$> joinT (applym theC ty1) (applym theC ty2)
  where
    ty1 = rename 1 t1
    ty2 = rename 2 t2

    toMatch1 = lstT ty1
    toMatch2 = fstT ty2

    joinT :: (Eq a, Show a) => GStmtType a -> GStmtType a -> TypeingOut (GStmtType a)
    joinT = joinT1 []

    joinT1 :: (Eq a, Show a) => [GStmtType a] -> GStmtType a -> GStmtType a -> TypeingOut (GStmtType a)
    joinT1 l (GaiwanArrow a@(GaiwanBuf size1 ta) b) c = joinT1 (a : l) b c
    --joinT1 l a (GaiwanArrow b c) | a == b = -- it just works?
    joinT1 l a@(GaiwanBuf size1 ta) (GaiwanArrow b@(GaiwanBuf size2 tb) c@(GaiwanBuf size3 tc)) | ta == tb = unifySize l a b c
    joinT1 l a@(GaiwanBuf size1 ta) (GaiwanArrow b c@(GaiwanArrow _ _)) = error $ "join with deep arry:" ++ show c
    joinT1 l a b = error $ "someting went wrong joining" ++ show (reverse (a : l), b, theC)

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
    listConstraints (a : ar) (b : bs) =
      let prev = constraints a b
       in let r = listConstraints (map (applym prev) ar) (map (applym prev) bs) in map (second (applym r)) prev ++ r
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

-- | Unify the size of buffers
unifySize :: [GStmtType a] -> GStmtType a -> GStmtType a -> GStmtType a -> TypeingOut (GStmtType a)
unifySize
  reversedInBufs
  (GaiwanBuf size2 _)
  (GaiwanBuf size3 _) -- buffers to be unified
  (GaiwanBuf size4 resType) =
    do
      cntSolution <-
        solveTCnt
          <$> mapM norm revCounts
            <*> norm size2
            <*> norm size3
            <*> norm size4
      uncurry buildOut <$> cntSolution
    where
      revCounts = map (\(GaiwanBuf size _) -> size) reversedInBufs

      buildOut argSizes endsize = foldZipRev reversedInBufs argSizes (GaiwanBuf (simplifyExp endsize) resType)

      foldZipRev [] [] c = c
      foldZipRev ((GaiwanBuf _ t) : lr) (s : sr) acc = foldZipRev lr sr $ GaiwanArrow (GaiwanBuf (simplifyExp s) t) acc

-- | Normalize a size expression and get (varname, a, b) = a*varname + b
-- TODO: make either
norm :: Exp -> TypeingOut (String, Int, Int)
norm (Times (Int a2) (Var name2 False)) = Right (name2, a2, 0)
norm (Times (Var name2 False) (Int a2)) = Right (name2, a2, 0)
norm (Var name2 False) = Right (name2, 1, 0)
norm (Plus (Var name2 False) (Int b)) = Right (name2, 1, b)
norm (Plus (Times (Var name2 False) (Int a)) (Int b)) = Right (name2, a, b)
norm (Plus (Times (Int a) (Var name2 False)) (Int b)) = Right (name2, a, b)
norm idk = Left $ show idk -- TODO

-- | solve
solveTCnt ::
  [(String, Int, Int)] ->
  (String, Int, Int) ->
  (String, Int, Int) ->
  (String, Int, Int) ->
  TypeingOut ([Exp], Exp)
solveTCnt
  sourceSizes -- input size
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified
  (name3, a3, b3) -- -/
  (name4, a4, b4) -- output size
    | name3 == name4 = do
      -- Solve modulo operation to find value for v
      v <-
        maybe
          (Left $ "Could not unify" ++ show (a2, b3 - b2, a3) ++ show (map (\v -> (a2 * v - (b3 - b2)) `mod` a3 == 0) [0 .. (abs a3)]))
          Right
          $ find (\v -> (a2 * v - (b3 - b2)) `mod` a3 == 0) [0 .. (abs a3)]

      unifiedSourceSizes <-
        (`mapM` sourceSizes) $ \(name1, a1, b1) ->
          if name1 == name2
            then Right $ Plus (Times (Int $ a1 * u) n) (Int $ b1 + a1 * v)
            else Left "All size varaibles for buffers in a single acion must be the same"
      let outputSize = Plus (Times (Int $ a4 * div (a2 * u) a3) n) (Int $ b4 + a4 * div (a2 * v + b2 - b3) a3)

      return (unifiedSourceSizes, outputSize)
    where
      n = Var name2 False
      u = abs $ div a3 (gcd a2 a3)

typeWithExpectionAndJustArgs :: Maybe StmtType -> [(String, Maybe StmtType)] -> Exp -> TypeingOut StmtType
typeWithExpectionAndJustArgs x args exp = maybe (Left "failed to lift maybe tuple") b (mapM liftMaybeTuple args)
  where
    b :: [(String, StmtType)] -> TypeingOut StmtType
    b actualArgs = typeWithExpection x actualArgs exp

typeWithExpection :: Maybe StmtType -> [(String, StmtType)] -> Exp -> TypeingOut StmtType
typeWithExpection x args exp = either Left (doRightCase x) (typeOfBody args exp)

doRightCase :: Maybe StmtType -> StmtType -> TypeingOut StmtType
doRightCase Nothing b = Right b
doRightCase (Just a) b | a == b = Right b
doRightCase (Just a) b = Left $ "Expected outtype does not match " ++ show a ++ " but got " ++ show b

typeOfBody :: [(String, StmtType)] -> Exp -> TypeingOut StmtType
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

typeOfVar :: [(String, StmtType)] -> Exp -> TypeingOut StmtType
typeOfVar _ (Var name True) = error "not implemented"
typeOfVar env (Var name False) = maybe (Left $ name ++ " not in env") Right $ lookup name env

typeOfMathBinop :: [(String, StmtType)] -> Exp -> Exp -> TypeingOut (GStmtType a)
typeOfMathBinop env a b = case mapM (typeOfBody env) [a, b] of
  Right [GaiwanInt, GaiwanInt] -> Right GaiwanInt
  Left e -> Left e
  Right t -> Left $ "failed to type math binop expected two ints, got " ++ show t

typeOfArrayAccess :: [(String, StmtType)] -> Exp -> Exp -> TypeingOut StmtType
typeOfArrayAccess env array index = case mapM (typeOfBody env) [array, index] of
  Right [GaiwanBuf _ ty, GaiwanInt] -> Right ty
  Right [GaiwanBuf _ ty, _] -> Left "failed to type array access: index is not an int"
  Right [at, GaiwanInt] -> Left $ "failed to type array access:" ++ show array ++ " was not an array but " ++ show at
  Right _ -> Left "failed to type array access: idk"
  Left e -> Left e

-- | Transform a Maybe to an Eiterh string
checkJust :: String -> Maybe a -> TypeingOut a
checkJust msg = maybe (Left msg) Right

checkSndJustList :: String -> [(b, Maybe a)] -> TypeingOut [(b, a)]
checkSndJustList msg a = checkJust msg $ mapM liftMaybeTuple a

liftMaybeTuple :: (a, Maybe b) -> Maybe (a, b)
liftMaybeTuple (a, Just x) = Just (a, x)
liftMaybeTuple (a, Nothing) = Nothing

bufferType1 :: Maybe StmtType -> TypeingOut (Maybe StmtType)
bufferType1 (Just (GaiwanBuf (Int 1) b)) = Right $ Just b
bufferType1 (Just GaiwanBuf {}) = Left "non buffer type with length one for out of reducer"
bufferType1 (Just _) = Left "non buffer type"
bufferType1 Nothing = Right Nothing

bufferType :: Maybe StmtType -> TypeingOut StmtType
bufferType (Just (GaiwanBuf _ b)) = Right b
bufferType (Just _) = Left "non buffer type"
bufferType Nothing = Left "no type given"

-- | Get the type of the elements of a buffer argument
liftMaybeBuffElemType :: (String, Maybe StmtType) -> TypeingOut (String, StmtType)
liftMaybeBuffElemType (a, b) = (a,) <$> bufferType b
