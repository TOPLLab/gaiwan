{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Language.GaiwanTypes
  ( Program (..),
    Stmt (..),
    StmtType (..),
    GAbsType (..),
    GShape (..),
    TypedStmt (..),
    VarFreeStmtType (..),
    GStmtTypeOrShape (..),
    GStmtTypeOrShapeDefault,
    stmtName,
    toTypedSmt,
    --    stmt,
    mergeT,
  )
where

import Code.Definitions
import Data.Bifunctor
import Data.Foldable
import Data.Maybe
import Language.GaiwanDefs

data GAbsType a
  = GaiwanArrow (GStmtTypeOrShape a) (GStmtTypeOrShape a)
  | GaiwanBuf Exp (GShape a)
  deriving (Show, Eq)

data Void

data GStmtTypeOrShape a = AType (GAbsType a) | AShape (GShape a) deriving (Show, Eq)

type GStmtTypeOrShapeDefault = GStmtTypeOrShape String

type EnvType = [(String, GStmtTypeOrShapeDefault)]

type MaybeEnvType = [(String, Maybe GStmtTypeOrShapeDefault)]

type VarFreeStmtType = GAbsType Void

type StmtType = GAbsType String

type StmtShape = GShape String

type Tag = (Int, String)

type TaggedStmtType = GStmtTypeOrShape Tag

type TypeingOut = Either String

-- eventually get rid of this... and make our own propper error messsages
instance MonadFail TypeingOut where
  fail = Left

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
  = Mapper (Maybe GStmtTypeOrShapeDefault) String [(String, Maybe GStmtTypeOrShapeDefault)] Exp
  | Reducer (Maybe GStmtTypeOrShapeDefault) String [(String, Maybe GStmtTypeOrShapeDefault)] Exp Exp
  | Shaper (Maybe GStmtTypeOrShapeDefault) String [(String, Maybe GStmtTypeOrShapeDefault)] Exp
  | Abstraction (Maybe GStmtTypeOrShapeDefault) String [(String, Maybe GStmtTypeOrShapeDefault)] [Stmt]
  deriving (Show, Eq)

stmt :: forall t. TypedStmt -> (StmtType -> String -> [String] -> t) -> t
stmt (TShaper a b c _) f = f a b c
stmt (TMapper a b c _) f = f a b c
stmt (TReducer a b c _ _) f = f a b c
stmt (TAbstraction a b c _) f = f a b c

stmtName :: TypedStmt -> String
stmtName x = stmt x (\_ name _ -> name)

maybeGaiwanInt :: (Eq a) => Maybe (GStmtTypeOrShape a) -> Bool
maybeGaiwanInt indexType = fromMaybe (AShape GaiwanInt) indexType == AShape GaiwanInt

toTypedSmt :: Stmt -> TypeingOut TypedStmt
toTypedSmt = toTypedSmtEnv []

toTypedSmtEnv :: EnvType -> Stmt -> TypeingOut TypedStmt
toTypedSmtEnv env (Abstraction outType name args []) = Left "Cannot make abstraction without content"
toTypedSmtEnv env (Abstraction outType name args parts) = do
  things <- checkSndJustList "all the arguments of an abstraction need to be set" args
  partType <- mapM (toTypedSmtEnv $ things ++ env) parts
  typedParts <- mergeTList $ map typedStmt partType
  (AType outT) <- doRightCase outType (AType typedParts)
  return $
    TAbstraction outT name (map fst args) partType
toTypedSmtEnv env (Mapper outType name args@[(indexArg, indexType), (dataVarName, Just (AShape dataVarType))] body)
  | maybeGaiwanInt indexType = do
    AShape retType <- typeWithExpection outType ([(indexArg, AShape GaiwanInt), (dataVarName, AShape dataVarType)] ++ env) body
    return $
      TMapper
        ( GaiwanArrow
            (AType (GaiwanBuf (Var "n" False) dataVarType))
            (AType (GaiwanBuf (Var "n" False) retType))
        )
        name
        (map fst args)
        body
toTypedSmtEnv env (Mapper outType name args body) = Left "Incorrect argument given to mapper"
toTypedSmtEnv env (Shaper outType@(Just (AType outTypeR@(GaiwanBuf _ elemType))) name args@((indexArg, indexType) : otherArgs) body)
  | maybeGaiwanInt indexType = do
    extraArgsBuf <- checkSndJustList "all the arguments of a shaper must have a specified buffer type" otherArgs
    extraArgs <- mapM liftMaybeBuffElemType otherArgs
    typeWithExpection (Just $ AShape elemType) (((indexArg, AShape GaiwanInt) : extraArgsBuf) ++ env) body
    return $
      TShaper
        (foldr (\n acc -> GaiwanArrow (snd n) (AType acc)) outTypeR extraArgsBuf)
        name
        (map fst args)
        body
toTypedSmtEnv env Shaper {} = Left "Incorrect argument given to Shaper"
toTypedSmtEnv env (Reducer outType name args@[(indexArg, indexType), (accArg, accType), (dataArg, dataType@(Just dataTypeR))] initExp body)
  | maybeGaiwanInt indexType = do
    AShape checkecAccType <- typeWithExpection accType env initExp
    if AShape checkecAccType == dataTypeR then return () else fail "Accumulator type and init expression are not of the same type"
    outElemType <- bufferType1 outType
    AShape checkedOutType <-
      typeWithExpectionAndJustArgs
        (AShape <$> outElemType)
        ( [(indexArg, Just $ AShape GaiwanInt), (accArg, Just $ AShape checkecAccType), (dataArg, dataType)] ++ map (second Just) env
        )
        body
    return $
      TReducer
        (GaiwanArrow (AType (GaiwanBuf (Var "n" False) checkecAccType)) (AType (GaiwanBuf (Int 1) checkedOutType)))
        name
        (map fst args)
        initExp
        body
toTypedSmtEnv env Reducer {} = Left "Incorrect argument given to Reducer"

-- | Paste a list of Statement Types onto each other
mergeTList :: [StmtType] -> TypeingOut StmtType
mergeTList [a, b] = mergeT a b
mergeTList [a] = Right a
mergeTList (a : ar) = mergeTList ar >>= mergeT a
mergeTList [] = Left "empty merge"

-- | merge two arrow types like function composition (and adjust buffer sizes if needed)
mergeT :: StmtType -> StmtType -> TypeingOut StmtType
mergeT t1 t2 = do
  theC <- constraints toMatch1 toMatch2
  res <- unrename <$> joinT (applym theC ty1) (applym theC ty2)
  case res of
    (AType x) -> return x
    b -> error "Unexpected output for joinT, was Shape, expected Type" -- should be dead code
  where
    ty1 = rename 1 (AType t1)
    ty2 = rename 2 (AType t2)

    toMatch1 = lstT ty1
    toMatch2 = fstT ty2

    joinT :: (Eq a, Show a) => GStmtTypeOrShape a -> GStmtTypeOrShape a -> TypeingOut (GStmtTypeOrShape a)
    joinT = joinT1 []

    joinT1 :: (Eq a, Show a) => [GAbsType a] -> GStmtTypeOrShape a -> GStmtTypeOrShape a -> TypeingOut (GStmtTypeOrShape a)
    joinT1 l (AType (GaiwanArrow (AType a@(GaiwanBuf size1 ta)) b)) c = joinT1 (a : l) b c
    --joinT1 l a (GaiwanArrow b c) | a == b = -- it just works?
    joinT1
      l
      (AType a@(GaiwanBuf size1 ta)) --                                                       SANITY
      (AType (GaiwanArrow (AType b@(GaiwanBuf size2 tb)) (AType c@(GaiwanBuf size3 tc)))) | ta == tb = do
        out <- unifySize l a b c
        return $ AType out
    joinT1 l (AType a@(GaiwanBuf size1 ta)) (AType (GaiwanArrow b (AType c@(GaiwanArrow _ _)))) = error $ "join with deep arry:" ++ show c
    joinT1 l (AType a) b = error $ "someting went wrong joining" ++ show (reverse (a : l), b)
    joinT1 l a b = error "someting went seriously wrong joining"

    lstT (AType (GaiwanArrow _ b)) = lstT b
    lstT a = a

    fstT (AType (GaiwanArrow b _)) = b

    constraints :: TaggedStmtType -> TaggedStmtType -> TypeingOut [(Tag, GShape Tag)]
    constraints (AShape a) (AShape b) = constraintss a b
    constraints (AType (GaiwanArrow a1 a2)) (AType (GaiwanArrow b1 b2)) = listConstraints [a1, a2] [b1, b2]
    constraints (AType (GaiwanBuf _ a)) (AType (GaiwanBuf _ b)) = constraintss a b
    constraints _ _ = Left "Could not match arguments"

    constraintss :: GShape Tag -> GShape Tag -> TypeingOut [(Tag, GShape Tag)]
    constraintss (TVar a) (TVar b) | a == b = Right []
    constraintss (TVar a) b = Right [(a, b)]
    constraintss a (TVar b) = Right [(b, a)]
    constraintss GaiwanInt GaiwanInt = Right []
    constraintss (GaiwanTuple a) (GaiwanTuple b) = listConstraints (map AShape a) (map AShape b)
    constraintss _ _ = Left "Could not match arguments"

    listConstraints :: [TaggedStmtType] -> [TaggedStmtType] -> TypeingOut [(Tag, GShape Tag)]
    listConstraints (a : ar) (b : bs) = do
      prev <- constraints a b
      r <- listConstraints (map (applym prev) ar) (map (applym prev) bs)
      return $ map (second (applyms r)) prev ++ r
    listConstraints [] [] = return []
    listConstraints _ _ = fail "unequal length in listConstraints"

    -- TODO: remove
    applym :: [(Tag, GShape Tag)] -> TaggedStmtType -> TaggedStmtType
    applym m = apply $ \name -> fromMaybe (TVar name) (lookup name m)

    applyms :: [(Tag, GShape Tag)] -> GShape Tag -> GShape Tag
    applyms m = applys $ \name -> fromMaybe (TVar name) (lookup name m)

    rename :: Int -> GStmtTypeOrShapeDefault -> TaggedStmtType
    rename v = apply $ \name -> TVar (v, name)

    apply :: (a -> GShape b) -> GStmtTypeOrShape a -> GStmtTypeOrShape b
    apply f (AShape xx) = AShape $ applys f xx
    apply f (AType (GaiwanArrow a1 a2)) = AType $ GaiwanArrow (apply f a1) (apply f a2)
    apply f (AType (GaiwanBuf n a)) = AType $ GaiwanBuf n $ applys f a

    -- apply for shapes only
    applys :: (a -> GShape b) -> GShape a -> GShape b
    applys f (TVar a) = f a
    applys f GaiwanInt = GaiwanInt
    applys f (GaiwanTuple a) = GaiwanTuple $ map (applys f) a

    unrename :: TaggedStmtType -> GStmtTypeOrShapeDefault
    unrename = apply $ \name -> TVar $ show name

-- | Unify the size of buffers
unifySize :: [GAbsType a] -> GAbsType a -> GAbsType a -> GAbsType a -> TypeingOut (GAbsType a)
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

      foldZipRev :: [GAbsType a] -> [Exp] -> GAbsType a -> GAbsType a
      foldZipRev [] [] c = c
      foldZipRev ((GaiwanBuf _ t) : bufferR) (s : sizeR) acc = foldZipRev bufferR sizeR $ GaiwanArrow (AType (GaiwanBuf (simplifyExp s) t)) (AType acc)
      foldZipRev _ _ _ = error "foldZipRev: aruments not of same length"
unifySize _ _ _ _ = Left "Cannot unify type on non buffers"

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
  TypeingOut ([Exp], Exp) -- (Sizes of the inputs, size of the output)
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
solveTCnt _ _ _ _ = Left "variable names in last arrowtype are not consistent"

typeWithExpectionAndJustArgs :: Maybe GStmtTypeOrShapeDefault -> MaybeEnvType -> Exp -> TypeingOut GStmtTypeOrShapeDefault
typeWithExpectionAndJustArgs x args exp = maybe (Left "failed to lift maybe tuple") b (mapM liftMaybeTuple args)
  where
    b :: EnvType -> TypeingOut GStmtTypeOrShapeDefault
    b actualArgs = typeWithExpection x actualArgs exp

typeWithExpection :: Maybe GStmtTypeOrShapeDefault -> EnvType -> Exp -> TypeingOut GStmtTypeOrShapeDefault
typeWithExpection x args exp = either Left (doRightCase x) (typeOfBody args exp)

doRightType :: (Eq a, Show a) => Maybe (GAbsType a) -> GAbsType a -> TypeingOut (GAbsType a)
doRightType a b = do
  AType out <- doRightCase (AType <$> a) (AType b)
  return out

doRightCase :: (Eq a, Show a) => Maybe (GStmtTypeOrShape a) -> GStmtTypeOrShape a -> TypeingOut (GStmtTypeOrShape a)
doRightCase Nothing b = Right b
doRightCase (Just a) b | a == b = Right b
doRightCase (Just a) b = Left $ "Expected outtype does not match " ++ show a ++ " but got " ++ show b

toShapeList :: [GStmtTypeOrShape String] -> TypeingOut [GShape String]
toShapeList ((AShape a) : r) = (a :) <$> toShapeList r
toShapeList [] = Right []
toShapeList _ = Left "Not all shape"

typeOfBody :: EnvType -> Exp -> TypeingOut GStmtTypeOrShapeDefault
typeOfBody env (Plus a b) = typeOfMathBinop env a b
typeOfBody env (Minus a b) = typeOfMathBinop env a b
typeOfBody env (Modulo a b) = typeOfMathBinop env a b
typeOfBody env (Times a b) = typeOfMathBinop env a b
typeOfBody env (Pow a b) = typeOfMathBinop env a b
typeOfBody env (Div a b) = typeOfMathBinop env a b
typeOfBody env (IsEq a b) = typeOfMathBinop env a b
typeOfBody env (IsGreater a b) = typeOfMathBinop env a b
typeOfBody env Let {} = Left "let not yet supported"
typeOfBody env (ArrayGet a b) = AShape <$> shapeOfArrayAccess env a b -- FIXME
typeOfBody env (Int _) = Right $ AShape GaiwanInt
typeOfBody env (Select t index) = case typeOfBody env t of
  Right (AShape (GaiwanTuple types)) | index >= 0 && index < length types -> Right $ AShape $ types !! index
  Right (AShape (GaiwanTuple types)) -> Left $ "Tuple index (" ++ show index ++ ") out of range"
  Right t -> Left $ "Can only select from Tuple, not from " ++ show t
  Left a -> Left a
typeOfBody env (Tuple exps) = AShape . GaiwanTuple <$> (mapM (typeOfBody env) exps >>= toShapeList)
typeOfBody env var@Var {} = typeOfVar env var
typeOfBody env (Negate e) = case typeOfBody env e of
  Right (AShape GaiwanInt) -> Right $ AShape GaiwanInt
  _ -> Left "Cannot negate"
typeOfBody env (If cond tBranch fBrach) = case mapM (typeOfBody env) [cond, tBranch, fBrach] of
  Right [tC, tT, fT] | tC == AShape GaiwanInt && tT == fT -> Right tT
  Right _ -> Left "Types of if did not match"
  Left x -> Left x
typeOfBody env PipedExp {} = Left "Cannot type a piped expression in a body"
typeOfBody env App {} = Left "I don't know this applicaion"
typeOfBody env GPUBufferGet {} = Left "Cannot use GPUBufferget in body"
typeOfBody env Loop {} = Left "Cannot use loop in body"

typeOfVar :: EnvType -> Exp -> TypeingOut (GStmtTypeOrShape String)
typeOfVar _ (Var name True) = error "not implemented"
typeOfVar env (Var name False) = maybe (Left $ name ++ " not in env") Right $ lookup name env
typeOfVar _ _ = Left "trying to typeOfVar on non variable"

typeOfMathBinop :: EnvType -> Exp -> Exp -> TypeingOut (GStmtTypeOrShape String)
typeOfMathBinop env a b = case mapM (typeOfBody env) [a, b] of
  Right [AShape GaiwanInt, AShape GaiwanInt] -> Right $ AShape GaiwanInt
  Left e -> Left e
  Right t -> Left $ "failed to type math binop expected two ints, got " ++ show t

shapeOfArrayAccess :: EnvType -> Exp -> Exp -> TypeingOut StmtShape
shapeOfArrayAccess env array index = case mapM (typeOfBody env) [array, index] of
  Right [AType (GaiwanBuf _ ty), AShape GaiwanInt] -> Right ty
  Right [AType (GaiwanBuf _ ty), _] -> Left "failed to type array access: index is not an int"
  Right [at, AShape GaiwanInt] -> Left $ "failed to type array access:" ++ show array ++ " was not an array but " ++ show at
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

bufferType1 :: Maybe GStmtTypeOrShapeDefault -> TypeingOut (Maybe StmtShape)
bufferType1 (Just (AType (GaiwanBuf (Int 1) b))) = Right $ Just b
bufferType1 (Just (AType GaiwanBuf {})) = Left "non buffer type with length one for out of reducer"
bufferType1 (Just _) = Left "non buffer type"
bufferType1 Nothing = Right Nothing

bufferType :: Maybe GStmtTypeOrShapeDefault -> TypeingOut StmtShape
bufferType (Just (AType (GaiwanBuf _ b))) = Right b
bufferType (Just _) = Left "non buffer type"
bufferType Nothing = Left "no type given"

-- | Get the type of the elements of a buffer argument
liftMaybeBuffElemType :: (String, Maybe GStmtTypeOrShapeDefault) -> TypeingOut (String, StmtShape)
liftMaybeBuffElemType (a, b) = (a,) <$> bufferType b
