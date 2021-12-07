{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Language.GaiwanTypes
  ( Program (..),
    TypedProgram (..),
    Stmt (..),
    AbstType (..),
    GAbsType (..),
    GTransformType (..),
    Abstraction (..),
    GShape (..),
    TypedTransform (..),
    TypedAbstraction (..),
    TypedInstr (..),
    VarFreeStmtType (..),
    GBufOrShape (..),
    GBufOrShapeDefault,
    StmtShape,
    GaiwanBuf (..),
    stmtName,
    toTypedSmt,
    --    stmt,
    mergeT,
    checkType,
    checkDefsType,
  )
where

import Code.Definitions
import Data.Bifunctor
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Language.GaiwanDefs

data Void -- needed for var free types

-- Type of a abstraction
data GAbsType a = GaiwanArrow [GShape a] (GTransformType a)
  deriving (Show, Eq)

-- Type of a buffer
data GaiwanBuf a = GaiwanBuf Exp (GShape a)
  deriving (Show, Eq)

type GaiwanBufDefault = GaiwanBuf String

-- Type of a transformation on buffers
data GTransformType a = GTransformType (Constraints a) [GaiwanBuf a] [GaiwanBuf a]
  deriving (Show, Eq)

type Constraints a = M.Map String (GaiwanBuf a)

data GBufOrShape a = ABuf (GaiwanBuf a) | AShape (GShape a) deriving (Show, Eq)

type GBufOrShapeDefault = GBufOrShape String

type TaggedBuff = GaiwanBuf Tag

type EnvType = [(String, GBufOrShapeDefault)]

type MaybeEnvType = [(String, Maybe GBufOrShapeDefault)]

type VarFreeStmtType = GAbsType Void

type AbstType = GAbsType String

type TransformType = GTransformType String

type StmtShape = GShape String

type Tag = (Int, String)

type TaggedStmtType = GBufOrShape Tag

type TypeingOut = Either String

-- eventually get rid of this... and make our own propper error messsages
instance MonadFail TypeingOut where
  fail = Left

data TypedAbstraction = TAbstraction AbstType String [String] [TypedTransform]
  deriving (Show, Eq)

data TypedTransform
  = TMapper TransformType String [String] Exp
  | TShaper TransformType String [String] Exp
  | TReducer TransformType String [String] Exp Exp
  deriving (Show, Eq)

data TypedInstr
  = TIApp TransformType TypedAbstraction [Exp] -- TODO: internals + let + cat
  | TLoop TransformType Exp String [TypedInstr]
  | TRetrun TransformType [String]
  | TLetB TransformType String [TypedInstr] [TypedInstr]
  deriving (Show, Eq)

typedInstr :: TypedInstr -> TransformType
typedInstr (TIApp t _ _) = t
typedInstr (TLoop t _ _ _) = t
typedInstr (TRetrun t _) = t
typedInstr (TLetB t _ _ _) = t

typedStmt :: TypedTransform -> TransformType
typedStmt (TMapper t _ _ _) = t
typedStmt (TShaper t _ _ _) = t
typedStmt (TReducer t _ _ _ _) = t

typedStmtName :: TypedTransform -> String
typedStmtName (TMapper _ t _ _) = t
typedStmtName (TShaper _ t _ _) = t
typedStmtName (TReducer _ t _ _ _) = t

newtype TypedProgram = TypedProg [TypedInstr]
  deriving (Show, Eq)

checkDefsType :: Program -> TypeingOut [TypedAbstraction]
checkDefsType (Prog s e) = mapM toTypedSmt s

checkType :: Program -> TypeingOut TypedProgram
checkType (Prog s e) = do
  typedStmts <- mapM toTypedSmt s -- type the definitions
  typedInstrs <- mapM (toTypedInstr typedStmts []) e -- apply the types of the definitions to the instrucions of the coordination language
  return $ TypedProg typedInstrs

toTypedInstr :: [TypedAbstraction] -> EnvType -> Instr -> TypeingOut TypedInstr
toTypedInstr definitions env (Return name) =
  let outType = GaiwanBuf (Var "freshname" False) (TVar "freshname") -- TODO
   in Right $ TRetrun (GTransformType (M.singleton name outType) [] [outType]) [name]
toTypedInstr definitions env (LetB name wl1 wl2) = do
  twl1b <- mapM (toTypedInstr definitions env) wl1
  twl1 <- mergeTList $ map typedInstr twl1b
  twl2b <- mapM (toTypedInstr definitions env) wl2
  twl2 <- mergeTList $ map typedInstr twl2b
  tout <- japply name twl1 twl2
  Right (TLetB tout name twl1b twl2b)
toTypedInstr definitions env (IApp "fresh" True [Int cnt]) =
  -- special shaper
  let outType = GTransformType M.empty [] [GaiwanBuf (Int cnt) GaiwanInt]
   in Right $ TIApp outType (TAbstraction (GaiwanArrow [] outType) "fresh" [] [TShaper outType "fresh" ["i"] (Var "i" False)]) []
toTypedInstr definitions env a@(IApp name True args) = Left $ "error: built in funtions not supported yet" ++ show a --TODO
toTypedInstr definitions env (IApp name False args) = do
  abstraction <- lookupAbst name definitions
  argTypes <- mapM (typeOfBody env) args
  apptype <- checkArgs (abstrType abstraction) argTypes
  return $ TIApp apptype abstraction args
toTypedInstr definitions env (Loop count varname childs) = do
  body <- mapM (toTypedInstr definitions ((varname, AShape GaiwanInt) : env)) childs
  looptype <- mergeTList (map typedInstr body)
  -- todo: size change?????
  -- todo: check if arrow type with same begin and end shape
  return $ TLoop looptype count varname body

-- | Check if the args are compatible with the statment and return the type of the application
japply :: String -> TransformType -> TransformType -> TypeingOut TransformType
japply name (GTransformType c1 [] [t1]) (GTransformType c2 [] t2) = do
  (newc, newt) <- constraintUnion t2 (M.insert name t1 c1) c2
  Right (GTransformType (M.delete name newc) [] newt)
japply _ _ _ = Left "Cannot japply with funciton as first argument"

constraintUnion :: [GaiwanBuf a] -> Constraints a -> Constraints a -> TypeingOut (Constraints a, [GaiwanBuf a])
constraintUnion outType c1 c2 = do
  let overlap = M.keys $ M.intersectionWith (,) c1 c2
  Left "..."

abstrType :: TypedAbstraction -> AbstType
abstrType (TAbstraction t _ _ _) = t

checkArgs :: AbstType -> [GBufOrShapeDefault] -> TypeingOut TransformType
checkArgs (GaiwanArrow from to) args | map AShape from == args = Right to
checkArgs fulltype@(GaiwanArrow from to) args = Left $ "Argument of bad type: " ++ show from ++ " but got  " ++ show args ++ "as an argument to " ++ show fulltype

lookupAbst :: String -> [TypedAbstraction] -> TypeingOut TypedAbstraction
lookupAbst name [] = Left $ "Could not find defintion for" ++ name
lookupAbst name (stmt : _) | name == abstrName stmt = Right stmt
lookupAbst name (_ : sr) = lookupAbst name sr

abstrName :: TypedAbstraction -> String
abstrName (TAbstraction _ name _ _) = name

data Program
  = Prog [Abstraction] [Instr]
  deriving (Show, Eq)

data Abstraction = Abstraction (Maybe GBufOrShapeDefault) String [(String, Maybe GBufOrShapeDefault)] [Stmt]
  deriving (Show, Eq)

data Stmt
  = Mapper (Maybe GBufOrShapeDefault) String [(String, Maybe GBufOrShapeDefault)] Exp
  | Reducer (Maybe GBufOrShapeDefault) String [(String, Maybe GBufOrShapeDefault)] Exp Exp
  | Shaper (Maybe GBufOrShapeDefault) String [(String, Maybe GBufOrShapeDefault)] Exp
  deriving (Show, Eq)

stmt :: forall t. TypedTransform -> (TransformType -> String -> [String] -> t) -> t
stmt (TShaper a b c _) f = f a b c
stmt (TMapper a b c _) f = f a b c
stmt (TReducer a b c _ _) f = f a b c

stmtName :: TypedTransform -> String
stmtName x = stmt x (\_ name _ -> name)

maybeGaiwanInt :: (Eq a) => Maybe (GBufOrShape a) -> Bool
maybeGaiwanInt indexType = fromMaybe (AShape GaiwanInt) indexType == AShape GaiwanInt

toTypedSmt :: Abstraction -> TypeingOut TypedAbstraction
toTypedSmt = toTypedAbst []

toTypedAbst :: EnvType -> Abstraction -> TypeingOut TypedAbstraction
toTypedAbst env (Abstraction outType name args []) = Left "Cannot make abstraction without content"
toTypedAbst env (Abstraction outType name args parts) = do
  things <- mapM checkAbstrArgs args
  partType <- mapM (toTypedSmtEnv $ map (second AShape) things ++ env) parts
  typedParts <- mergeTList $ map typedStmt partType
  outT <- checkExpected outType ((\(GTransformType constraints _ [gbs]) -> ABuf gbs) typedParts) -- TODO: with case handle exceptions
  return $
    TAbstraction (GaiwanArrow (map snd things) typedParts) name (map fst args) partType

checkAbstrArgs :: (String, Maybe GBufOrShapeDefault) -> Either String (String, GShape String)
checkAbstrArgs (name, Nothing) = Left "all the arguments of an abstraction need to be set"
checkAbstrArgs (name, Just (ABuf _)) = Left "Abstractions can only take scalars, not buffers"
checkAbstrArgs (name, Just (AShape shape)) = Right (name, shape)

toTypedSmtEnv :: EnvType -> Stmt -> TypeingOut TypedTransform
toTypedSmtEnv env (Mapper outType name args@[(indexArg, indexType), (dataVarName, Just (AShape dataVarType))] body)
  | maybeGaiwanInt indexType = do
    AShape retType <- typeWithExpection outType ([(indexArg, AShape GaiwanInt), (dataVarName, AShape dataVarType)] ++ env) body
    return $
      TMapper
        ( GTransformType
            M.empty
            [GaiwanBuf (Var "n" False) dataVarType]
            [GaiwanBuf (Var "n" False) retType]
        )
        name
        (map fst args)
        body
toTypedSmtEnv env (Mapper outType name args body) = Left "Incorrect argument given to mapper"
toTypedSmtEnv env (Shaper outType@(Just (ABuf outTypeR@(GaiwanBuf _ elemType))) name args@((indexArg, indexType) : otherArgs) body)
  | maybeGaiwanInt indexType = do
    extraArgsBuf <- checkSndJustList "all the arguments of a shaper must have a specified buffer type" otherArgs
    extraArgs <- mapM liftMaybeBuff otherArgs
    typeWithExpection (Just $ AShape elemType) (((indexArg, AShape GaiwanInt) : extraArgsBuf) ++ env) body
    return $
      TShaper
        (GTransformType M.empty (map snd extraArgs) [outTypeR])
        name
        (map fst args)
        body
toTypedSmtEnv env s@(Shaper _ name ((indexArg, indexType) : otherArgs) _) | maybeGaiwanInt indexType = Left $ "Invalid out type for shaper, expected a buffer for " ++ show s
toTypedSmtEnv env s@Shaper {} = Left $ "Incorrect argument given to " ++ show s
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
        (GTransformType M.empty [GaiwanBuf (Var "n" False) checkecAccType] [GaiwanBuf (Int 1) checkedOutType])
        name
        (map fst args)
        initExp
        body
toTypedSmtEnv env Reducer {} = Left "Incorrect argument given to Reducer"

-- | Paste a list of Statement Types onto each other
mergeTList :: [TransformType] -> TypeingOut TransformType
mergeTList [a, b] = mergeT a b
mergeTList [a] = Right a
mergeTList (a : ar) = mergeTList ar >>= mergeT a
mergeTList [] = Left "empty merge"

-- | merge two tranformationTypes like function composition (and adjust buffer sizes if needed)
mergeT :: TransformType -> TransformType -> TypeingOut TransformType
mergeT t1 t2 = do
  toMatch1 <- lstT ty1
  toMatch2 <- fstT ty2
  theC <- constraints toMatch1 toMatch2
  -- adjust buffersizes and undo rename
  unrename <$> joinT (applym theC ty1) (applym theC ty2)
  where
    -- TranformationTyped tagged
    ty1 = rename 1 t1
    ty2 = rename 2 t2 -- TODO check no constraints
    lstT (GTransformType _ from to) = Right to
    fstT (GTransformType _ from to) = Right from

joinT :: (Eq a, Show a) => GTransformType a -> GTransformType a -> TypeingOut (GTransformType a)
joinT (GTransformType c1 from1 to1) (GTransformType c2 from2 to2) | null c2 && length to1 == length from2 = joinT1 RecordType {toAdjustC = c1, toAdjustB = from1, toMatch = to1} RecordType {toAdjustC = c2, toAdjustB = to2, toMatch = from2}
joinT GTransformType {} GTransformType {} = Left "incompatible number of args"

data RecordType a = RecordType
  { toAdjustB :: [GaiwanBuf a],
    toAdjustC :: Constraints a,
    toMatch :: [GaiwanBuf a]
  }

joinT1 :: RecordType a -> RecordType a -> TypeingOut (GTransformType a)
joinT1
  RecordType {toAdjustC = mc1, toAdjustB = from1, toMatch = to1}
  RecordType {toAdjustC = mc2, toAdjustB = to2, toMatch = from2} = do
    -- TODO: make nicer
    arg1 <- mapM normBufSize from1
    arg2 <- mapM normBufSize to1
    arg3 <- mapM normBufSize from2
    arg4 <- mapM normBufSize to2
    let c1 = M.toAscList mc1
    cTypes <- mapM (normBufSize . snd) c1
    (cTypesOut, fFrom, fTo) <- joinT2 cTypes arg1 arg2 arg3 arg4
    let c2 = zip (map fst c1) (zipWith reJoin (map snd c1) cTypesOut) -- TODO check
    return (GTransformType (M.fromAscList c2) (zipWith reJoin from1 fFrom) (zipWith reJoin to2 fTo))
    where
      reJoin :: GaiwanBuf a -> (String, Int, Int) -> GaiwanBuf a
      reJoin (GaiwanBuf exp gs) newSize = GaiwanBuf (denorm newSize) gs

joinT2 :: [(String, Int, Int)] -> [(String, Int, Int)] -> [(String, Int, Int)] -> [(String, Int, Int)] -> [(String, Int, Int)] -> TypeingOut ([(String, Int, Int)], [(String, Int, Int)], [(String, Int, Int)])
joinT2 c f [] [] t = Right (c, f, t)
joinT2 c f (l1 : lr) (r1 : rr) t = do
  (l, r) <- solveTCnt l1 r1
  fN <- mapM l f
  tN <- mapM r t
  lN <- mapM l lr
  rN <- mapM r rr
  cN <- mapM l c -- check when does not apply TODO should just work instread of fail
  joinT2 cN fN lN rN tN
joinT2 _ f _ _ t = Left "incompatible number of buffers"

normBufSize :: GaiwanBuf a -> TypeingOut (String, Int, Int)
normBufSize (GaiwanBuf size _) = norm size

-- Constrainst for the types (exlucting type)
constraints :: [TaggedBuff] -> [TaggedBuff] -> TypeingOut [(Tag, GShape Tag)]
constraints a b = listConstraints (map bufType a) (map bufType b)

constraintss :: GShape Tag -> GShape Tag -> TypeingOut [(Tag, GShape Tag)]
constraintss (TVar a) (TVar b) | a == b = Right []
constraintss (TVar a) b = Right [(a, b)]
constraintss a (TVar b) = Right [(b, a)]
constraintss GaiwanInt GaiwanInt = Right []
constraintss (GaiwanTuple a) (GaiwanTuple b) = listConstraints a b
constraintss _ _ = Left "Could not match arguments"

listConstraints :: [GShape Tag] -> [GShape Tag] -> TypeingOut [(Tag, GShape Tag)]
listConstraints (a : ar) (b : bs) = do
  prev <- constraintss a b
  r <- listConstraints (map (applyms prev) ar) (map (applyms prev) bs)
  return $ map (second (applyms r)) prev ++ r
listConstraints [] [] = return []
listConstraints _ _ = fail "unequal length in listConstraints"

applym :: [(Tag, GShape Tag)] -> GTransformType Tag -> GTransformType Tag
applym m (GTransformType c gbs gbs') = GTransformType (M.map (applyb m) c) (map (applyb m) gbs) (map (applyb m) gbs')

applyb :: [(Tag, GShape Tag)] -> GaiwanBuf Tag -> GaiwanBuf Tag
applyb m (GaiwanBuf exp gs) = GaiwanBuf exp (applyms m gs)

applyms :: [(Tag, GShape Tag)] -> GShape Tag -> GShape Tag
applyms m = applys $ \name -> fromMaybe (TVar name) (lookup name m)

rename :: Int -> TransformType -> GTransformType Tag
rename v (GTransformType c gbs gbs') = GTransformType (M.map (renameb v) c) (map (renameb v) gbs) (map (renameb v) gbs')

renamec :: Int -> Constraints String -> Constraints Tag
renamec v = M.map (renameb v)

renameb :: Int -> GaiwanBufDefault -> TaggedBuff
renameb v = apply $ \name -> TVar (v, name)

apply :: (a -> GShape b) -> GaiwanBuf a -> GaiwanBuf b
apply f (GaiwanBuf exp gs) = GaiwanBuf exp (applys f gs)

-- apply for shapes only
applys :: (a -> GShape b) -> GShape a -> GShape b
applys f (TVar a) = f a
applys f GaiwanInt = GaiwanInt
applys f (GaiwanTuple a) = GaiwanTuple $ map (applys f) a

unrename :: GTransformType Tag -> TransformType
unrename (GTransformType c gbs gb) = GTransformType (M.map unrenameb c) (map unrenameb gbs) (map unrenameb gb)

unrenameb :: TaggedBuff -> GaiwanBufDefault
unrenameb = apply $ \name -> TVar $ show name

-- | Unify the size of buffers
bufType :: TaggedBuff -> GShape Tag
bufType (GaiwanBuf exp gs) = gs

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

denorm :: (String, Int, Int) -> Exp
denorm (name, a, k) = simplifyExp $ Plus (Times (Int a) (Var name False)) (Int k)

-- | solve
solveTCnt ::
  (String, Int, Int) ->
  (String, Int, Int) ->
  TypeingOut ((String, Int, Int) -> TypeingOut (String, Int, Int), (String, Int, Int) -> TypeingOut (String, Int, Int)) -- (Sizes of the inputs, sizes of the output)
solveTCnt
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified
  (name3, a3, b3) -- -/
    =
    do
      -- Solve modulo operation to find value for v
      v <-
        maybe
          (Left $ "Could not unify" ++ show (a2, b3 - b2, a3) ++ show (map (\v -> (a2 * v - (b3 - b2)) `mod` a3 == 0) [0 .. (abs a3)]))
          Right
          $ find (\v -> (a2 * v - (b3 - b2)) `mod` a3 == 0) [0 .. (abs a3)]
      let leftTransformer = \(name1, a1, b1) ->
            if name1 == name2
              then Right (name2, a1 * u, b1 + a1 * v)
              else Left "All size varaibles for buffers in a single acion must be the same"
      let rightTransformer = \(name4, a4, b4) ->
            if name4 == name3
              then Right (name3, a4 * div (a2 * u) a3, b4 + a4 * div (a2 * v + b2 - b3) a3)
              else Left "All size varaibles for buffers in a single acion must be the same"
      return (leftTransformer, rightTransformer)
    where
      n = Var name2 False
      u = abs $ div a3 (gcd a2 a3)

typeWithExpectionAndJustArgs :: Maybe GBufOrShapeDefault -> MaybeEnvType -> Exp -> TypeingOut GBufOrShapeDefault
typeWithExpectionAndJustArgs x args exp = maybe (Left "failed to lift maybe tuple") b (mapM liftMaybeTuple args)
  where
    b :: EnvType -> TypeingOut GBufOrShapeDefault
    b actualArgs = typeWithExpection x actualArgs exp

typeWithExpection :: Maybe GBufOrShapeDefault -> EnvType -> Exp -> TypeingOut GBufOrShapeDefault
typeWithExpection x args exp = either Left (checkExpected x) (typeOfBody args exp)

checkExpected :: (Eq a, Show a) => Maybe (GBufOrShape a) -> GBufOrShape a -> TypeingOut (GBufOrShape a)
checkExpected Nothing b = Right b
checkExpected (Just a) b | a == b = Right b
checkExpected (Just a) b = Left $ "Expected outtype does not match " ++ show a ++ " but got " ++ show b

toShapeList :: [GBufOrShape String] -> TypeingOut [GShape String]
toShapeList ((AShape a) : r) = (a :) <$> toShapeList r
toShapeList [] = Right []
toShapeList _ = Left "Not all shape"

typeOfBody :: EnvType -> Exp -> TypeingOut GBufOrShapeDefault
typeOfBody env (Plus a b) = typeOfMathBinop env a b
typeOfBody env (Minus a b) = typeOfMathBinop env a b
typeOfBody env (Modulo a b) = typeOfMathBinop env a b
typeOfBody env (Times a b) = typeOfMathBinop env a b
typeOfBody env (Pow a b) = typeOfMathBinop env a b
typeOfBody env (Div a b) = typeOfMathBinop env a b
typeOfBody env (IsEq a b) = typeOfMathBinop env a b
typeOfBody env (IsGreater a b) = typeOfMathBinop env a b
typeOfBody env (Let varname value exp) = typeOfBody env value >>= (\t -> typeOfBody ((varname, t) : env) exp)
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
typeOfBody env a@App {} = Left $ "I don't know this applicaion" ++ show a
typeOfBody env GPUBufferGet {} = Left "Cannot use GPUBufferget in body"

typeOfVar :: EnvType -> Exp -> TypeingOut (GBufOrShape String)
typeOfVar _ (Var name True) = error "not implemented"
typeOfVar env (Var name False) = maybe (Left $ name ++ " not in env") Right $ lookup name env
typeOfVar _ _ = Left "trying to typeOfVar on non variable"

typeOfMathBinop :: EnvType -> Exp -> Exp -> TypeingOut (GBufOrShape String)
typeOfMathBinop env a b = case mapM (typeOfBody env) [a, b] of
  Right [AShape GaiwanInt, AShape GaiwanInt] -> Right $ AShape GaiwanInt
  Left e -> Left e
  Right t -> Left $ "failed to type math binop expected two ints, got " ++ show t ++ " for " ++ show [a, b]

shapeOfArrayAccess :: EnvType -> Exp -> Exp -> TypeingOut StmtShape
shapeOfArrayAccess env array index = case mapM (typeOfBody env) [array, index] of
  Right [ABuf (GaiwanBuf _ ty), AShape GaiwanInt] -> Right ty
  Right [ABuf (GaiwanBuf _ ty), _] -> Left "failed to type array access: index is not an int"
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

bufferType1 :: Maybe GBufOrShapeDefault -> TypeingOut (Maybe StmtShape)
bufferType1 (Just (ABuf (GaiwanBuf (Int 1) b))) = Right $ Just b
bufferType1 (Just (ABuf GaiwanBuf {})) = Left "non buffer type with length one for out of reducer"
bufferType1 (Just _) = Left "non buffer type"
bufferType1 Nothing = Right Nothing

-- | Get the type of the elements of a buffer argument
liftMaybeBuff :: (String, Maybe GBufOrShapeDefault) -> TypeingOut (String, GaiwanBufDefault)
liftMaybeBuff (a, Nothing) = Left "No type given"
liftMaybeBuff (a, Just (ABuf buf)) = Right (a, buf)
liftMaybeBuff (a, Just (AShape _)) = Left "Got scalar, expeced buffer"
