{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Language.GaiwanTypes
  ( Program (..),
    TypedProgram (..),
    AbstType (..),
    GAbsType (..),
    GTransformType (..),
    GShape (..),
    TypedTransform (..),
    TypedAbstraction (..),
    TypedInstr (..),
    GBufOrShape (..),
    GBufOrShapeDefault,
    StmtShape,
    GaiwanBuf (..),
    GaiwanBufSize (..),
    stmtName,
    toTypedSmtSimple,
    --    stmt,
    mergeT,
    checkType,
    backPropagate,
    checkDefsType,
  )
where

import Code.Definitions
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Foldable
import Data.Functor
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Language.AlphaRename
-- import Debug.Trace
import Language.GaiwanDefs

-- Type of a buffer

type GaiwanBufDefault = GaiwanBuf ShapeVar

-- Type of a transformation on buffers
-- (constrainsts are only used for typing retun, GTransformType contraints [] outBufs)
type TransformType = GTransformType ShapeVar

type GBufOrShapeDefault = GBufOrShape ShapeVar

type TaggedBuff a = GaiwanBuf (Tag a)

type EnvType = [(String, GBufOrShapeDefault)]

type MaybeEnvType = [(String, Maybe GBufOrShapeDefault)]

type AbstType = GAbsType ShapeVar

type ShapeVar = Int

type StmtShape = GShape ShapeVar

type Tag a = (Int, a)

type TaggedStmtType a = GBufOrShape (Tag a)

class Eq a => Tagable a where
  untagify :: Tag a -> a

instance Freshable ShapeVar where
  fresh = nextUniqv

instance (Eq a, Freshable a) => Freshable (GaiwanBufSize a) where
  fresh = do
    varName <- fresh
    return $ GaiwanBufSize varName 1 0

instance (Eq a, Freshable a) => Freshable (GShape a) where
  fresh = TVar <$> fresh

instance Tagable String where
  untagify (tag, name) = name ++ "@" ++ show tag

instance Tagable ShapeVar where
  untagify (tag, name) | tag < 10 = 10 * name + tag -- FIXME not needed as names will be unique
  untagify _ = error "illegal tagging"

data TypedAbstraction = TAbstraction AbstType String [String] [TypedTransform]
  deriving (Show, Eq)

data TypedTransform
  = TMapper TransformType String [String] Exp
  | TShaper TransformType String [String] Exp
  | TReducer TransformType String [String] Exp Exp
  deriving (Show, Eq)

data TypedInstr
  = TIApp TransformType TypedAbstraction [Exp]
  | TLoop TransformType Exp String [TypedInstr]
  | TReturn TransformType [String]
  | TLetB TransformType String [TypedInstr] [TypedInstr]
  deriving (Show, Eq)

typedInstr :: TypedInstr -> TransformType
typedInstr (TIApp t _ _) = t
typedInstr (TLoop t _ _ _) = t
typedInstr (TReturn t _) = t
typedInstr (TLetB t _ _ _) = t

typedStmt :: TypedTransform -> TransformType
typedStmt (TMapper t _ _ _) = t
typedStmt (TShaper t _ _ _) = t
typedStmt (TReducer t _ _ _ _) = t

typedStmtName :: TypedTransform -> String
typedStmtName (TMapper _ t _ _) = t
typedStmtName (TShaper _ t _ _) = t
typedStmtName (TReducer _ t _ _ _) = t

data TypedProgram = TypedProg TransformType [TypedInstr]
  deriving (Show, Eq)

checkDefsType :: Program String -> Either String [TypedAbstraction]
checkDefsType (Prog s e) =
  (`evalStateT` 0) $ do
    renamedS <- mapM alphaRename s
    mapM toTypedSmt renamedS

checkType :: Program String -> Either String TypedProgram
checkType (Prog s e) = (`evalStateT` 0) $ do
  renamedS <- mapM alphaRename s
  typedStmts <- mapM toTypedSmt renamedS -- type the definitions
  typedInstrs <- mapM (toTypedInstr typedStmts []) e -- apply the types of the definitions to the instrucions of the coordination language
  resultType <- mergeTList (map typedInstr typedInstrs)
  return $ TypedProg resultType typedInstrs

nextUniqvBuf :: TypeingOut (GaiwanBuf ShapeVar)
nextUniqvBuf = do
  linSize <- fresh
  GaiwanBuf linSize <$> fresh

toTypedInstr :: [TypedAbstraction] -> EnvType -> Instr -> TypeingOut TypedInstr
toTypedInstr definitions env (Return names) = do
  -- TODO what the same buffer is returned twice?
  outTypes <- mapM (const nextUniqvBuf) names
  return $ TReturn (GTransformType (M.fromList (zip names outTypes)) [] outTypes) names
toTypedInstr definitions env (LetB name wl1 wl2) = do
  twl1b <- mapM (toTypedInstr definitions env) wl1
  twl1 <- mergeTList $ map typedInstr twl1b
  twl2b <- mapM (toTypedInstr definitions env) wl2
  twl2 <- mergeTList $ map typedInstr twl2b
  tout <- japply name twl1 twl2
  return (TLetB tout name twl1b twl2b)
toTypedInstr definitions env (IApp "fresh" True [Int cnt]) = do
  -- special shaper
  varName <- fresh
  let outType = GTransformType M.empty [] [GaiwanBuf (GaiwanBufSize varName 0 cnt) GaiwanInt]
  return $ TIApp outType (TAbstraction (GaiwanArrow [] outType) "fresh" [] [TShaper outType "fresh" ["i"] (Var "i" False)]) []
toTypedInstr definitions env a@(IApp name True args) = fail $ "error: built in funtions not supported yet" ++ show a -- TODO
toTypedInstr definitions env (IApp name False args) = do
  abstraction <- lookupAbst name definitions
  argTypes <- mapM (typeOfBody env) args
  -- TODO should these be reverserd? first fill in shape vars in abstType?
  absType <- alphaRenameAbstType $ abstrType abstraction
  apptype <- checkArgs absType argTypes
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
  (newc, newt) <- constraintUnion (M.insert name t1 c1) c2 t2
  return (GTransformType (M.delete name newc) [] newt)
japply _ _ _ = fail "Cannot japply with funciton as first argument"

-- | Unify constrains in args
-- @c1@ and @c2@ will be unified and the resulting changes will also be applied to @outtype@
-- Rerurns the new constrains and the new outtype
constraintUnion :: (Show a, Freshable a, Tagable a) => Constraints a -> Constraints a -> [GaiwanBuf a] -> TypeingOut (Constraints a, [GaiwanBuf a])
constraintUnion c1 c2 outtype = do
  let overlap = M.keys c1 `L.intersect` M.keys c2
  c1vals <- maybe (fail "err") return $ mapM (`M.lookup` c1) overlap
  c2vals <- maybe (fail "err") return $ mapM (`M.lookup` c2) overlap
  let combinedTagedConstraints = M.union (M.map (renameb 1) c1) (M.map (renameb 2) c2)
  GTransformType c1c2new c1c2valsnew outtypenew <-
    unrename
      <$> mergeTTaged
        (GTransformType combinedTagedConstraints (map (renameb 1 . snd) $ M.toAscList c1) (map (renameb 1) c1vals))
        (rename 2 (GTransformType M.empty c2vals outtype))
  return (c1c2new, outtypenew)

abstrType :: TypedAbstraction -> AbstType
abstrType (TAbstraction t _ _ _) = t

checkArgs :: AbstType -> [GBufOrShapeDefault] -> TypeingOut TransformType
checkArgs (GaiwanArrow from to) args | map AShape from == args = return to
checkArgs fulltype@(GaiwanArrow from to) args = fail $ "Argument of bad type: " ++ show from ++ " but got  " ++ show args ++ "as an argument to " ++ show fulltype

lookupAbst :: String -> [TypedAbstraction] -> TypeingOut TypedAbstraction
lookupAbst name [] = fail $ "Could not find defintion for" ++ name
lookupAbst name (stmt : _) | name == abstrName stmt = return stmt
lookupAbst name (_ : sr) = lookupAbst name sr

abstrName :: TypedAbstraction -> String
abstrName (TAbstraction _ name _ _) = name

type ProgramDefault = Program ShapeVar

type StmtDefault = Stmt ShapeVar

type AbstractionDefault = Abstraction ShapeVar

stmt :: forall t. TypedTransform -> (TransformType -> String -> [String] -> t) -> t
stmt (TShaper a b c _) f = f a b c
stmt (TMapper a b c _) f = f a b c
stmt (TReducer a b c _ _) f = f a b c

stmtName :: TypedTransform -> String
stmtName x = stmt x (\_ name _ -> name)

maybeGaiwanInt :: (Eq a) => Maybe (GBufOrShape a) -> Bool
maybeGaiwanInt indexType = fromMaybe (AShape GaiwanInt) indexType == AShape GaiwanInt

toTypedSmtSimple :: Abstraction String -> Either String TypedAbstraction
toTypedSmtSimple abs =
  (`evalStateT` 0) $ do
    renamedS <- alphaRename abs
    toTypedSmt renamedS

toTypedSmt :: AbstractionDefault -> TypeingOut TypedAbstraction
toTypedSmt = toTypedAbst []

toTypedAbst :: EnvType -> AbstractionDefault -> TypeingOut TypedAbstraction
toTypedAbst env (Abstraction outType name args []) = fail "Cannot make abstraction without content"
toTypedAbst env (Abstraction outType name args parts) = do
  things <- mapM checkAbstrArgs args
  partType <- mapM (toTypedSmtEnv $ map (second AShape) things ++ env) parts
  typedParts <- mergeTList $ map typedStmt partType
  outT <- checkExpected outType ((\(GTransformType constraints _ [gbs]) -> ABuf gbs) typedParts)
  return $
    TAbstraction (GaiwanArrow (map snd things) typedParts) name (map fst args) partType

checkAbstrArgs :: (String, Maybe GBufOrShapeDefault) -> TypeingOut (String, GShape ShapeVar)
checkAbstrArgs (name, Nothing) = fail "all the arguments of an abstraction need to be set"
checkAbstrArgs (name, Just (ABuf _)) = fail "Abstractions can only take scalars, not buffers"
checkAbstrArgs (name, Just (AShape shape)) = return (name, shape)

toTypedSmtEnv :: EnvType -> StmtDefault -> TypeingOut TypedTransform
toTypedSmtEnv env (Mapper outType name args@[(indexArg, indexType), (dataVarName, Just (AShape dataVarType))] body)
  | maybeGaiwanInt indexType = do
      AShape retType <- typeWithExpection outType ([(indexArg, AShape GaiwanInt), (dataVarName, AShape dataVarType)] ++ env) body
      linBufSize <- fresh
      return $
        TMapper
          ( GTransformType
              M.empty
              [GaiwanBuf linBufSize dataVarType]
              [GaiwanBuf linBufSize retType]
          )
          name
          (map fst args)
          body
toTypedSmtEnv env (Mapper outType name args body) = fail "Incorrect argument given to mapper"
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
toTypedSmtEnv env s@(Shaper _ name ((indexArg, indexType) : otherArgs) _) | maybeGaiwanInt indexType = fail $ "Invalid out type for shaper, expected a buffer for " ++ show s
toTypedSmtEnv env s@Shaper {} = fail $ "Incorrect argument given to " ++ show s
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
      varName <- fresh
      return $
        TReducer
          (GTransformType M.empty [GaiwanBuf (GaiwanBufSize varName 1 0) checkecAccType] [GaiwanBuf (GaiwanBufSize varName 0 1) checkedOutType])
          name
          (map fst args)
          initExp
          body
toTypedSmtEnv env Reducer {} = fail "Incorrect argument given to Reducer"

-- | Paste a list of Statement Types onto each other
mergeTList :: [TransformType] -> TypeingOut TransformType
mergeTList [a, b] = mergeT a b
mergeTList [a] = return a
mergeTList (a : ar) = mergeTList ar >>= mergeT a
mergeTList [] = fail "empty merge"

-- | merge two tranformationTypes like function composition (and adjust buffer sizes if needed)
mergeT :: (Tagable a, Show a, Freshable a) => GTransformType a -> GTransformType a -> TypeingOut (GTransformType a)
mergeT t1 t2 =
  unrename <$> mergeTTaged (rename 1 t1) (rename 2 t2)

mergeTTaged :: (Tagable a, Show a, Freshable a) => GTransformType (Tag a) -> GTransformType (Tag a) -> TypeingOut (GTransformType (Tag a))
mergeTTaged ty1@(GTransformType c1 from1 to1) ty2@(GTransformType c2 from2 to2) | M.null c2 = do
  theC <- constraints to1 from2
  -- apply the solution of the merge of the type vars and then adjust buffersizes
  joinT (applym theC ty1) (applym theC ty2)
mergeTTaged _ _ = fail "cannot merge with constraint on the right"

joinT :: (Tagable a, Show a, Freshable a) => GTransformType (Tag a) -> GTransformType (Tag a) -> TypeingOut (GTransformType (Tag a))
joinT (GTransformType c1 from1 to1) (GTransformType c2 from2 to2)
  | null c2 && length to1 == length from2 =
      joinT1
        RecordType {toAdjustC = c1, toAdjustB = from1, toMatch = to1}
        RecordType {toAdjustC = c2, toAdjustB = to2, toMatch = from2}
joinT GTransformType {} GTransformType {} = fail "incompatible number of args"

data RecordType a = RecordType
  { toAdjustB :: [GaiwanBuf a],
    toAdjustC :: Constraints a,
    toMatch :: [GaiwanBuf a]
  }

joinT1 :: (Freshable a, Show a, Eq a) => RecordType (Tag a) -> RecordType (Tag a) -> TypeingOut (GTransformType (Tag a))
joinT1
  RecordType {toAdjustC = mc1, toAdjustB = from1, toMatch = to1}
  RecordType {toAdjustC = mc2, toAdjustB = to2, toMatch = from2} | M.null mc2 = do
    -- TODO: make nicer
    arg1 <- mapM normBufSize from1
    arg2 <- mapM normBufSize to1
    arg3 <- mapM normBufSize from2
    arg4 <- mapM normBufSize to2
    let c1 = M.toAscList mc1
    cTypes <- mapM (normBufSize . snd) c1
    (cTypesOut, fFrom, fTo) <- joinT3 cTypes arg1 arg2 arg3 arg4
    let c1new = zip (map fst c1) (zipWith reJoin (map snd c1) cTypesOut)
    return (GTransformType (M.fromAscList c1new) (zipWith reJoin from1 fFrom) (zipWith reJoin to2 fTo))
    where
      reJoin :: GaiwanBuf a -> (a, Int, Int) -> GaiwanBuf a
      reJoin (GaiwanBuf exp gs) newSize = GaiwanBuf (denorm newSize) gs
joinT1 _ _ = fail "mc2 was non empty"

joinT3 ::
  (Freshable a, Eq a, Show a) =>
  -- |  constraint acc
  [(Tag a, Int, Int)] ->
  -- | from acc
  [(Tag a, Int, Int)] ->
  -- | from todo
  [(Tag a, Int, Int)] ->
  -- | to todo
  [(Tag a, Int, Int)] ->
  -- | to acc
  [(Tag a, Int, Int)] ->
  TypeingOut ([(Tag a, Int, Int)], [(Tag a, Int, Int)], [(Tag a, Int, Int)])
joinT3 c from fm tm to = do
  f <- joinT2 fm tm return
  c' <- mapM f c
  from' <- mapM f from
  to' <- mapM f to
  return (c', from', to')

joinT2 ::
  (Freshable a, Eq a, Show a) =>
  -- | from todo
  [(Tag a, Int, Int)] ->
  -- | to todo
  [(Tag a, Int, Int)] ->
  -- unifier accumulator
  ((Tag a, Int, Int) -> TypeingOut (Tag a, Int, Int)) ->
  TypeingOut ((Tag a, Int, Int) -> TypeingOut (Tag a, Int, Int))
joinT2 [] [] f = return f
joinT2 (l1 : lr) (r1 : rr) f = do
  nextF <- solveTCnt l1 r1
  lN <- mapM (nextF) lr
  rN <- mapM (nextF) rr
  joinT2 lN rN (\a -> (f a) >>= nextF) -- TODO make chaining nicer
joinT2 _ _ f = fail "incompatible number of buffers"

-- | solve
solveTCnt ::
  (Freshable a, Eq a, Show a) =>
  (Tag a, Int, Int) ->
  (Tag a, Int, Int) ->
  TypeingOut ((Tag a, Int, Int) -> TypeingOut (Tag a, Int, Int))
solveTCnt
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified
  (name3, a3, b3) -- -/
    | name2 == name3 && a2 == a3 && b2 == b3 =
        return (return) -- basically id
solveTCnt
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified TODO improve docs FIXME CHECK THIS !!!!
  (name3, a3, b3) -- -/
    | b2 == b3 && (a2 == 0 || a3 == 0) =
        -- we know that the linear factor must be zero (0*x + b = a₃*x + b ⇒ a₃ = 0)
        do
          uniqv <- fresh
          let zeroTransformer = \(name1, a1, b1) ->
                if name1 == name2 || a1 == 0
                  then return ((4, uniqv), 0, b1)
                  else return (name1, a1, b1) -- leave unchanged
          return zeroTransformer
solveTCnt
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified TODO improve docs
  (name3, a3, b3) -- -/
    | name2 == name3 =
        fail $ "oh no " ++ show ((name2, a2, b2), (name3, a3, b3))
solveTCnt
  (name2, a2, b2) -- RHS is constant
  (name3, 0, b3) = fail $ show ((name2, a2, b2), (name3, 0, b3))
solveTCnt
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified
  (name3, a3, b3) -- -/
    =
    do
      -- Solve modulo operation to find value for v
      uniqv <- fresh

      v <-
        if b2 == b3
          then return 0
          else
            maybe
              (fail $ "Could not unify" ++ show (a2, b3 - b2, a3))
              return
              $ find (\v -> (a2 * v - (b3 - b2)) `mod` a3 == 0) [0 .. (abs a3)]
      return
        ( \(otherName, a1, b1) ->
            case () of
              _
                | otherName == name2 ->
                    ( -- trace (show assertName ++ "Unified with left " ++ show (otherName, a1, b1) ++ " with " ++ show (name2, a2, b2) ++ " to " ++ show ((3, uniqv), a1 * u, b1 + a1 * v)) $
                      return ((3, uniqv), a1 * u, b1 + a1 * v)
                    )
                | otherName == name3 ->
                    ( -- trace ( show assertName ++ "Unified with right " ++ show (otherName, a1, b1) ++ " with " ++ show (name3, a3, b3) ++ " to " ++ show ((3, uniqv), a1 * div (a2 * u) a3, b1 + a1 * div (a2 * v + b2 - b3) a3)) $
                      return ((3, uniqv), a1 * div (a2 * u) a3, b1 + a1 * div (a2 * v + b2 - b3) a3)
                    )
                | otherwise ->
                    -- trace (show assertName ++ "Unified with nothing " ++ show (otherName, a1, b1) ++ " remains") $
                    return (otherName, a1, b1)
        )
    where
      u = abs $ div a3 (gcd a2 a3)

normBufSize :: GaiwanBuf a -> TypeingOut (a, Int, Int)
normBufSize (GaiwanBuf size _) = norm size

-- Constrainst for the types (ignoring sizes)
constraints :: Eq a => [TaggedBuff a] -> [TaggedBuff a] -> TypeingOut [(Tag a, GShape (Tag a))]
constraints a b = listConstraints (map bufType a) (map bufType b)

constraintss :: Eq a => GShape (Tag a) -> GShape (Tag a) -> TypeingOut [(Tag a, GShape (Tag a))]
constraintss (TVar a) (TVar b) | a == b = return []
constraintss (TVar a) b = return [(a, b)]
constraintss a (TVar b) = return [(b, a)]
constraintss GaiwanInt GaiwanInt = return []
constraintss (GaiwanTuple a) (GaiwanTuple b) = listConstraints a b
constraintss _ _ = fail "Could not match arguments"

listConstraints :: Eq a => [GShape (Tag a)] -> [GShape (Tag a)] -> TypeingOut [(Tag a, GShape (Tag a))]
listConstraints (a : ar) (b : bs) = do
  prev <- constraintss a b
  r <- listConstraints (map (applyms prev) ar) (map (applyms prev) bs)
  return $ map (second (applyms r)) prev ++ r
listConstraints [] [] = return []
listConstraints _ _ = fail "unequal length in listConstraints"

-- | Apply typevar constraint solution of Transformtype (inculding contraints)
applym :: Eq a => [(Tag a, GShape (Tag a))] -> GTransformType (Tag a) -> GTransformType (Tag a)
applym m (GTransformType c gbs gbs') = GTransformType (M.map (applyb m) c) (map (applyb m) gbs) (map (applyb m) gbs')

applyb :: Eq a => [(Tag a, GShape (Tag a))] -> GaiwanBuf (Tag a) -> GaiwanBuf (Tag a)
applyb m (GaiwanBuf exp gs) = GaiwanBuf exp (applyms m gs)

applyms :: Eq a => [(Tag a, GShape (Tag a))] -> GShape (Tag a) -> GShape (Tag a)
applyms m = applys $ \name -> fromMaybe (TVar name) (lookup name m)

rename :: Int -> GTransformType a -> GTransformType (Tag a)
rename v (GTransformType c gbs gbs') = GTransformType (M.map (renameb v) c) (map (renameb v) gbs) (map (renameb v) gbs')

renamec :: Int -> Constraints a -> Constraints (Tag a)
renamec v = M.map (renameb v)

renameb :: Int -> GaiwanBuf a -> TaggedBuff a
renameb v = apply (\name -> TVar (v, name)) (\(GaiwanBufSize a n i) -> GaiwanBufSize (v, a) n i)

apply :: (a -> GShape b) -> (GaiwanBufSize a -> GaiwanBufSize b) -> GaiwanBuf a -> GaiwanBuf b
apply f fs (GaiwanBuf exp gs) = GaiwanBuf (fs exp) (applys f gs)

-- apply for shapes only
applys :: (a -> GShape b) -> GShape a -> GShape b
applys f (TVar a) = f a
applys f GaiwanInt = GaiwanInt
applys f (GaiwanTuple a) = GaiwanTuple $ map (applys f) a

unrename :: Tagable a => GTransformType (Tag a) -> GTransformType a
unrename (GTransformType c gbs gb) = GTransformType (M.map unrenameb c) (map unrenameb gbs) (map unrenameb gb)

unrenameb :: Tagable a => GaiwanBuf (Tag a) -> GaiwanBuf a
unrenameb = apply (TVar . untagify) (\(GaiwanBufSize x0 n i) -> GaiwanBufSize (untagify x0) n i)

-- | Unify the size of buffers
bufType :: TaggedBuff a -> GShape (Tag a)
bufType (GaiwanBuf exp gs) = gs

-- | Normalize a size expression and get (varname, a, b) = a*varname + b
norm :: GaiwanBufSize a -> TypeingOut (a, Int, Int)
norm (GaiwanBufSize name slope intercept) = return (name, slope, intercept)

denorm :: (a, Int, Int) -> GaiwanBufSize a
denorm (name, slope, intercept) = GaiwanBufSize name slope intercept

typeWithExpectionAndJustArgs :: Maybe GBufOrShapeDefault -> MaybeEnvType -> Exp -> TypeingOut GBufOrShapeDefault
typeWithExpectionAndJustArgs x args exp = maybe (fail "failed to lift maybe tuple") b (mapM liftMaybeTuple args)
  where
    b :: EnvType -> TypeingOut GBufOrShapeDefault
    b actualArgs = typeWithExpection x actualArgs exp

typeWithExpection :: Maybe GBufOrShapeDefault -> EnvType -> Exp -> TypeingOut GBufOrShapeDefault
typeWithExpection x args exp = do
  res <- typeOfBody args exp
  checkExpected x res

checkExpected :: (Eq a, Show a) => Maybe (GBufOrShape a) -> GBufOrShape a -> TypeingOut (GBufOrShape a)
checkExpected Nothing b = return b
checkExpected (Just a) b | a == b = return b
checkExpected (Just a) b = fail $ "Expected outtype does not match " ++ show a ++ " but got " ++ show b

toShapeList :: [GBufOrShape a] -> TypeingOut [GShape a]
toShapeList ((AShape a) : r) = (a :) <$> toShapeList r
toShapeList [] = return []
toShapeList _ = fail "Not all shape"

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
typeOfBody env (Int _) = return $ AShape GaiwanInt
typeOfBody env (Select t index) = do
  res <- typeOfBody env t
  case res of
    (AShape (GaiwanTuple types)) | index >= 0 && index < length types -> return $ AShape $ types !! index
    (AShape (GaiwanTuple types)) -> fail $ "Tuple index (" ++ show index ++ ") out of range"
    t -> fail $ "Can only select from Tuple, not from " ++ show t
typeOfBody env (Tuple exps) = AShape . GaiwanTuple <$> (mapM (typeOfBody env) exps >>= toShapeList)
typeOfBody env var@Var {} = typeOfVar env var
typeOfBody env (Negate e) = do
  res <- typeOfBody env e
  case res of
    (AShape GaiwanInt) -> return $ AShape GaiwanInt
    _ -> fail "Cannot negate"
typeOfBody env (If cond tBranch fBrach) = do
  res <- mapM (typeOfBody env) [cond, tBranch, fBrach]
  case res of
    [tC, tT, fT] | tC == AShape GaiwanInt && tT == fT -> return tT
    _ -> fail "Types of if did not match"
typeOfBody env GPUBufferGet {} = fail "Cannot use GPUBufferget in body"

typeOfVar :: EnvType -> Exp -> TypeingOut (GBufOrShape ShapeVar)
typeOfVar _ (Var name True) = error "not implemented"
typeOfVar env (Var name False) = maybe (fail $ name ++ " not in env") return $ lookup name env
typeOfVar _ _ = fail "trying to typeOfVar on non variable"

typeOfMathBinop :: EnvType -> Exp -> Exp -> TypeingOut GBufOrShapeDefault
typeOfMathBinop env a b = do
  res <- mapM (typeOfBody env) [a, b]
  case res of
    [AShape GaiwanInt, AShape GaiwanInt] -> return $ AShape GaiwanInt
    t -> fail $ "failed to type math binop expected two ints, got " ++ show t ++ " for " ++ show [a, b]

shapeOfArrayAccess :: EnvType -> Exp -> Exp -> TypeingOut StmtShape
shapeOfArrayAccess env array index = do
  res <- mapM (typeOfBody env) [array, index]
  case res of
    [ABuf (GaiwanBuf _ ty), AShape GaiwanInt] -> return ty
    [ABuf (GaiwanBuf _ ty), _] -> fail "failed to type array access: index is not an int"
    [at, AShape GaiwanInt] -> fail $ "failed to type array access:" ++ show array ++ " was not an array but " ++ show at
    _ -> fail "Crititical failure! TODO FIXME"

-- | Transform a Maybe to an Eiterh string
checkJust :: String -> Maybe a -> TypeingOut a
checkJust msg = maybe (fail msg) return

checkSndJustList :: String -> [(b, Maybe a)] -> TypeingOut [(b, a)]
checkSndJustList msg a = checkJust msg $ mapM liftMaybeTuple a

liftMaybeTuple :: (a, Maybe b) -> Maybe (a, b)
liftMaybeTuple (a, Just x) = Just (a, x)
liftMaybeTuple (a, Nothing) = Nothing

bufferType1 :: Maybe GBufOrShapeDefault -> TypeingOut (Maybe StmtShape)
bufferType1 (Just (ABuf (GaiwanBuf (GaiwanBufSize sizeName slope intercept) b))) | slope == 0 && intercept == 1 = return $ Just b
bufferType1 (Just (ABuf GaiwanBuf {})) = fail "non buffer type with length one for out of reducer"
bufferType1 (Just _) = fail "non buffer type given to reducer"
bufferType1 Nothing = return Nothing

-- | Get the type of the elements of a buffer argument
liftMaybeBuff :: (String, Maybe GBufOrShapeDefault) -> TypeingOut (String, GaiwanBufDefault)
liftMaybeBuff (a, Nothing) = fail "No type given"
liftMaybeBuff (a, Just (ABuf buf)) = return (a, buf)
liftMaybeBuff (a, Just (AShape _)) = fail "Got scalar, expeced buffer"

data BackPropMap a = BackPropMap -- the same product type with record syntax
  { bpmShapesMap :: M.Map a (GShape a),
    bpmSizes :: (GaiwanBufSize a -> GaiwanBufSize a)
  }

bpmBufShape :: BackPropMap ShapeVar -> GShape ShapeVar -> GShape ShapeVar
bpmBufShape bpm GaiwanInt = GaiwanInt
bpmBufShape bpm (GaiwanTuple gss) = GaiwanTuple $ map (bpmBufShape bpm) gss
bpmBufShape bpm (TVar shape) = (bpmShapesMap bpm) M.! shape

emptyBPM = BackPropMap M.empty id

backPropagate :: TypedProgram -> TypeingOut TypedProgram
backPropagate (TypedProg t typedInstr) = TypedProg t <$> (backPropagateT emptyBPM t typedInstr)

-- NOTE: The returntype given as 2nd argumetn may not be correct!!!
backPropagateT :: BackPropMap ShapeVar -> TransformType -> [TypedInstr] -> TypeingOut [TypedInstr]
backPropagateT _ _ [] = return []
backPropagateT
  lt
  (GTransformType constr1 fromT1 toT1)
  ((TIApp (GTransformType constr2 fromT2 toT2) ta exps) : tis) =
    do
      ltc <- createLTConstrainst lt constr1 constr2
      ltt <- createLTTypes ltc fromT1 fromT2
      fromT2N <- applyBPT ltt fromT2
      toT2N <- applyBPT ltt toT2
      let resType = (GTransformType constr1 fromT2N toT2N)
      let nextType = (GTransformType constr1 toT2N toT1)
      tisN <- backPropagateT ltt nextType tis
      taN <- backPropagateA emptyBPM resType ta
      return $ ((TIApp resType taN exps) : tisN)
backPropagateT
  lt
  (GTransformType constr1 fromT1 toT1)
  ((TLoop (GTransformType constr2 fromT2 toT2) exp s loopTis) : tis) =
    do
      ltc <- createLTConstrainst lt constr1 constr2
      ltt <- createLTTypes ltc fromT1 fromT2
      fromT2N <- applyBPT ltt fromT2
      toT2N <- applyBPT ltt toT2
      let resType = (GTransformType constr1 fromT2N toT2N)
      let nextType = (GTransformType constr1 toT2N toT1)
      tisN <- backPropagateT ltt nextType tis
      loopTisN <- backPropagateT emptyBPM resType loopTis
      return $ ((TLoop resType exp s loopTisN) : tisN)
backPropagateT
  lt
  (GTransformType constr1 [] toT1)
  ((TReturn (GTransformType constr2 [] toT2) ss) : tis) =
    do
      ltc <- createLTConstrainst lt constr1 constr2
      toT2N <- applyBPT ltc toT2
      let nextType = (GTransformType constr1 toT2N toT1)
      tisN <- backPropagateT ltc nextType tis
      return $ ((TReturn (GTransformType constr1 [] toT2N) ss) : tisN)
backPropagateT lt _ ((TReturn {}) : tis) =
  fail "Invalid requirement or derived return type"
backPropagateT
  lt
  (GTransformType constr1 [] toT1)
  ((TLetB (GTransformType constr2 [] toT2) varName tisVal tis2Body) : tis) =
    do
      ltc <- createLTConstrainst lt constr1 constr2
      toT2N <- applyBPT ltc toT2
      let nextType = (GTransformType constr1 toT2N toT1)
      tisN <- backPropagateT ltc nextType tis
      tisValN <- backPropagateT emptyBPM (GTransformType constr1 [] [] {- wrong constrains should be enough -}) tisVal
      let (GTransformType _ _ [tisValNRet]) = typedInstr (last tisValN)
      let bodyConstr = M.insert varName tisValNRet (M.delete varName constr1)
      tisBodyN <- backPropagateT emptyBPM (GTransformType bodyConstr [] toT1) tisVal
      return $ ((TLetB (GTransformType constr1 [] toT2N) varName tisValN tisBodyN) : tisN)
backPropagateT lt _ ((TLetB {}) : tis) =
  fail "Invalid requirement or derived letB type"

-- TODO clean up
backPropagateA :: BackPropMap ShapeVar -> GTransformType ShapeVar -> TypedAbstraction -> TypeingOut TypedAbstraction
backPropagateA
  bpm
  (GTransformType constr1 fromT1 _)
  (TAbstraction (GaiwanArrow scalars (GTransformType constr2 fromT2 toT2)) name argNames steps) =
    do
      ltc <- createLTConstrainst bpm constr1 constr2
      ltt <- createLTTypes ltc fromT1 fromT2
      fromT2N <- applyBPT ltt fromT2
      toT2N <- applyBPT ltt toT2
      let nextType = (GTransformType constr2 fromT2N toT2N)
      stepsN <- backPropagateS emptyBPM nextType steps
      return (TAbstraction (GaiwanArrow scalars nextType) name argNames stepsN)

-- constraints do not mater for our transformers
-- TODO remove duplication !
backPropagateS :: BackPropMap ShapeVar -> GTransformType ShapeVar -> [TypedTransform] -> TypeingOut [TypedTransform]
backPropagateS bpm (GTransformType _ fromT1 _) [] = return []
backPropagateS
  bpm
  (GTransformType _ fromT1 _)
  ((TMapper (GTransformType _ fromT2 toT2) s ss exp) : steps) =
    do
      ltt <- createLTTypes bpm fromT1 fromT2
      fromT2N <- applyBPT ltt fromT2
      toT2N <- applyBPT ltt toT2
      let nextType = (GTransformType M.empty toT2N [])
      stepsN <- backPropagateS emptyBPM nextType steps
      return ((TMapper (GTransformType M.empty fromT2N toT2N) s ss exp) : stepsN)
backPropagateS
  bpm
  (GTransformType _ fromT1 _)
  ((TShaper (GTransformType _ fromT2 toT2) s ss exp) : steps) =
    do
      ltt <- createLTTypes bpm fromT1 fromT2
      fromT2N <- applyBPT ltt fromT2
      toT2N <- applyBPT ltt toT2
      let nextType = (GTransformType M.empty toT2N [])
      stepsN <- backPropagateS emptyBPM nextType steps
      return ((TShaper (GTransformType M.empty fromT2N toT2N) s ss exp) : stepsN)
backPropagateS
  bpm
  (GTransformType _ fromT1 _)
  ((TReducer (GTransformType _ fromT2 toT2) s ss exp exp') : steps) =
    do
      ltt <- createLTTypes bpm fromT1 fromT2
      fromT2N <- applyBPT ltt fromT2
      toT2N <- applyBPT ltt toT2
      let nextType = (GTransformType M.empty toT2N [])
      stepsN <- backPropagateS emptyBPM nextType steps
      return ((TReducer (GTransformType M.empty fromT2N toT2N) s ss exp exp') : steps)

applyBPT :: BackPropMap ShapeVar -> [GaiwanBuf ShapeVar] -> TypeingOut [GaiwanBuf ShapeVar]
applyBPT lt = mapM (applyBPT1 lt)

applyBPT1 :: BackPropMap ShapeVar -> GaiwanBuf ShapeVar -> TypeingOut (GaiwanBuf ShapeVar)
applyBPT1 bpm (GaiwanBuf size shape) = return $ GaiwanBuf (bpmSizes bpm size) (bpmBufShape bpm shape)

createLTTypes :: BackPropMap ShapeVar -> [GaiwanBuf ShapeVar] -> [GaiwanBuf ShapeVar] -> TypeingOut (BackPropMap ShapeVar)
createLTTypes bpm [] [] = return bpm
createLTTypes
  bpm
  ((GaiwanBuf sizeConc shapeConc) : concreters)
  ((GaiwanBuf sizeWeak shapeWeak) : weakers) =
    do
      bpm1 <- extendBPMSize bpm sizeConc sizeWeak
      bpm2 <- extendBPMShape bpm1 shapeConc shapeWeak
      createLTTypes bpm2 concreters weakers
createLTTypes bpm _ _ = fail "Incompatible nuber of joined buffers, this should not happen"

-- Note that we do not need to care about collisions in the map :)
extendBPMShape :: BackPropMap ShapeVar -> GShape ShapeVar -> GShape ShapeVar -> TypeingOut (BackPropMap ShapeVar)
extendBPMShape bpm GaiwanInt GaiwanInt = return bpm
extendBPMShape bpm (GaiwanTuple concreter) (GaiwanTuple weaker) = foldrM (\a b -> extendBPMShape b (fst a) (snd a)) bpm (zip concreter weaker)
extendBPMShape bpm b (TVar a) = return $ bpm {bpmShapesMap = M.insert a b (bpmShapesMap bpm)}
extendBPMShape _ a b = fail $ "lol incompatible" ++ (show (a, b))

-- xa+b = yc+d    =>   y = a/c + (b-d)/c
extendBPMSize :: Eq a => BackPropMap a -> GaiwanBufSize a -> GaiwanBufSize a -> TypeingOut (BackPropMap a)
extendBPMSize bpm@BackPropMap {bpmSizes = oldF} (GaiwanBufSize x a b) (GaiwanBufSize y c d) =
  return $
    bpm
      { bpmSizes = \i@(GaiwanBufSize z e f) ->
          if z == y
            then (GaiwanBufSize x ((a * e) `div` c) ((((b - d) * e) `div` c) + f))
            else (oldF i)
      }

-- intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c

createLTConstrainst :: BackPropMap ShapeVar -> Constraints ShapeVar -> Constraints ShapeVar -> TypeingOut (BackPropMap ShapeVar)
createLTConstrainst bpm c1 c2 = uncurry (createLTTypes bpm) $ L.unzip $ M.elems $ M.intersectionWith (\a b -> (a, b)) c1 c2
