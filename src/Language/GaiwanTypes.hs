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
    TypeingOut,
    VarFreeStmtType (..),
    GBufOrShape (..),
    GBufOrShapeDefault,
    StmtShape,
    GaiwanBuf (..),
    stmtName,
    toTypedSmtSimple,
    --    stmt,
    mergeT,
    checkType,
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
import Language.GaiwanDefs

data Void -- needed for var free types

-- Type of a abstraction
data GAbsType a = GaiwanArrow [GShape a] (GTransformType a)
  deriving (Show, Eq)

-- Type of a buffer
data GaiwanBuf a = GaiwanBuf Exp (GShape a)
  deriving (Show, Eq)

type GaiwanBufDefault = GaiwanBuf ShapeVar

-- Type of a transformation on buffers
data GTransformType a = GTransformType (Constraints a) [GaiwanBuf a] [GaiwanBuf a]
  deriving (Show, Eq)

type Constraints a = M.Map String (GaiwanBuf a)

data GBufOrShape a = ABuf (GaiwanBuf a) | AShape (GShape a) deriving (Show, Eq)

type GBufOrShapeDefault = GBufOrShape ShapeVar

type TaggedBuff a = GaiwanBuf (Tag a)

type EnvType = [(String, GBufOrShapeDefault)]

type MaybeEnvType = [(String, Maybe GBufOrShapeDefault)]

type VarFreeStmtType = GAbsType Void

type AbstType = GAbsType ShapeVar

type ShapeVar = Int

type TransformType = GTransformType ShapeVar

type StmtShape = GShape ShapeVar

type Tag a = (Int, a)

type TaggedStmtType a = GBufOrShape (Tag a)

type TypeingOut = StateT Int (Either String)

instance MonadFail (Either String) where
  fail = Left

class Eq a => Tagable a where
  untagify :: Tag a -> a

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

data TypedProgram = TypedProg TransformType [TypedInstr]
  deriving (Show, Eq)

checkDefsType :: Program String -> Either String [TypedAbstraction]
checkDefsType (Prog s e) =
  (`evalStateT` 0) $ do
    renamedS <- mapM alphaRenameShape s
    mapM toTypedSmt renamedS

checkType :: Program String -> Either String TypedProgram
checkType (Prog s e) = (`evalStateT` 0) $ do
  renamedS <- mapM alphaRenameShape s
  typedStmts <- mapM toTypedSmt renamedS -- type the definitions
  typedInstrs <- mapM (toTypedInstr typedStmts []) e -- apply the types of the definitions to the instrucions of the coordination language
  resultType <- mergeTList (map typedInstr typedInstrs)
  return $ TypedProg resultType typedInstrs

alphaRenameShape :: Abstraction String -> TypeingOut AbstractionDefault
alphaRenameShape (Abstraction m_gbos name args steps) = do
  steps' <- mapM doAlphaRename steps
  lt <- createAlphaLT args
  m_gbos' <- applyAphaLTMaybe lt m_gbos
  args' <- mapM (applyAphaLTArg lt) args
  return $ Abstraction m_gbos' name args' steps'
  where
    doAlphaRename :: Stmt String -> TypeingOut (Stmt ShapeVar)
    doAlphaRename (Mapper m_gbos str x0 exp) = do
      lt' <- createAlphaLT x0
      m_gbos' <- applyAphaLTMaybe lt' m_gbos
      x0' <- mapM (applyAphaLTArg lt') x0
      return $ Mapper m_gbos' str x0' exp
    doAlphaRename (Reducer m_gbos str x0 exp exp') = do
      lt' <- createAlphaLT x0
      m_gbos' <- applyAphaLTMaybe lt' m_gbos
      x0' <- mapM (applyAphaLTArg lt') x0
      return $ Reducer m_gbos' str x0' exp exp'
    doAlphaRename (Shaper m_gbos str x0 exp) = do
      lt' <- createAlphaLT x0
      m_gbos' <- applyAphaLTMaybe lt' m_gbos
      x0' <- mapM (applyAphaLTArg lt') x0
      return $ Shaper m_gbos' str x0' exp

    applyAphaLTArg :: M.Map String ShapeVar -> (String, Maybe (GBufOrShape String)) -> TypeingOut (String, Maybe (GBufOrShape ShapeVar))
    applyAphaLTArg lt (name, ty) = do
      v <- applyAphaLTMaybe lt ty
      return (name, v)

    applyAphaLTMaybe :: M.Map String ShapeVar -> Maybe (GBufOrShape String) -> TypeingOut (Maybe (GBufOrShape ShapeVar))
    applyAphaLTMaybe lt Nothing = return Nothing
    applyAphaLTMaybe lt (Just (ABuf (GaiwanBuf exp gs))) = applyAphaLT lt gs <&> (Just . ABuf . GaiwanBuf exp)
    applyAphaLTMaybe lt (Just (AShape gs)) = applyAphaLT lt gs <&> (Just . AShape)

    applyAphaLT :: M.Map String ShapeVar -> GShape String -> TypeingOut (GShape ShapeVar)
    applyAphaLT lt GaiwanInt = return GaiwanInt
    applyAphaLT lt (GaiwanTuple gss) = mapM (applyAphaLT lt) gss <&> GaiwanTuple
    applyAphaLT lt (TVar a) = maybe (fail $ "Could not find " ++ a ++ " in applyAphaLT " ++ (show lt)) (return . TVar) (M.lookup a lt)

    createAlphaLT = growAlphaLT M.empty

    growAlphaLT :: M.Map String ShapeVar -> ArgList String -> TypeingOut (M.Map String ShapeVar)
    growAlphaLT lt [] = return lt
    growAlphaLT lt ((s, Nothing) : xr) = growAlphaLT lt xr
    growAlphaLT lt ((s, Just bufOrShape) : xr) = do
      lt' <- growLTSingle (collectShape bufOrShape) lt
      growAlphaLT lt' xr

    collectShape :: GBufOrShape a -> GShape a
    collectShape (ABuf (GaiwanBuf _ gs)) = gs
    collectShape (AShape gs) = gs

    growLTSingle :: (Ord s) => GShape s -> M.Map s ShapeVar -> TypeingOut (M.Map s ShapeVar)
    growLTSingle GaiwanInt lt = return lt
    growLTSingle (GaiwanTuple gss) lt = foldrM growLTSingle lt gss
    growLTSingle (TVar str) lt | M.member str lt = return lt
    growLTSingle (TVar str) lt = do
      newName <- nextUniqv
      return $ M.insert str newName lt

nextUniqv :: TypeingOut Int
nextUniqv = do
  uniqv <- get
  put (uniqv + 1)
  return uniqv

nextUniqvBuf :: TypeingOut (GaiwanBuf ShapeVar)
nextUniqvBuf = do
  uniqv_len <- nextUniqv
  GaiwanBuf (Var ("freshlen_" ++ show uniqv_len) False) . TVar <$> nextUniqv

toTypedInstr :: [TypedAbstraction] -> EnvType -> Instr -> TypeingOut TypedInstr
toTypedInstr definitions env (Return names) = do
  outTypes <- mapM (const nextUniqvBuf) names
  return $ TRetrun (GTransformType (M.fromList (zip names outTypes)) [] outTypes) names
toTypedInstr definitions env (LetB name wl1 wl2) = do
  twl1b <- mapM (toTypedInstr definitions env) wl1
  twl1 <- mergeTList $ map typedInstr twl1b
  twl2b <- mapM (toTypedInstr definitions env) wl2
  twl2 <- mergeTList $ map typedInstr twl2b
  tout <- japply name twl1 twl2
  return (TLetB tout name twl1b twl2b)
toTypedInstr definitions env (IApp "fresh" True [Int cnt]) = do
  -- special shaper
  let outType = GTransformType M.empty [] [GaiwanBuf (Int cnt) GaiwanInt]
  return $ TIApp outType (TAbstraction (GaiwanArrow [] outType) "fresh" [] [TShaper outType "fresh" ["i"] (Var "i" False)]) []
toTypedInstr definitions env a@(IApp name True args) = fail $ "error: built in funtions not supported yet" ++ show a --TODO
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
  (newc, newt) <- constraintUnion (M.insert name t1 c1) c2 t2
  return (GTransformType (M.delete name newc) [] newt)
japply _ _ _ = fail "Cannot japply with funciton as first argument"

-- | Unify constrains in args
-- @c1@ and @c2@ will be unified and the resulting changes will also be applied to @outtype@
-- Rerurns the new constrains and the new outtype
constraintUnion :: Tagable a => Constraints a -> Constraints a -> [GaiwanBuf a] -> TypeingOut (Constraints a, [GaiwanBuf a])
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

data Program a
  = Prog [Abstraction a] [Instr]
  deriving (Show, Eq)

data Abstraction a = Abstraction (Maybe (GBufOrShape a)) String (ArgList a) [Stmt a]
  deriving (Show, Eq)

type ProgramDefault = Program ShapeVar

type StmtDefault = Stmt ShapeVar

type AbstractionDefault = Abstraction ShapeVar

type ArgList a = [(String, Maybe (GBufOrShape a))]

data Stmt a
  = Mapper (Maybe (GBufOrShape a)) String (ArgList a) Exp
  | Reducer (Maybe (GBufOrShape a)) String (ArgList a) Exp Exp
  | Shaper (Maybe (GBufOrShape a)) String (ArgList a) Exp
  deriving (Show, Eq)

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
    renamedS <- alphaRenameShape abs
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
    return $
      TReducer
        (GTransformType M.empty [GaiwanBuf (Var "n" False) checkecAccType] [GaiwanBuf (Int 1) checkedOutType])
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
mergeT :: Tagable a => GTransformType a -> GTransformType a -> TypeingOut (GTransformType a)
mergeT t1 t2 =
  unrename <$> mergeTTaged (rename 1 t1) (rename 2 t2)

mergeTTaged :: Tagable a => GTransformType (Tag a) -> GTransformType (Tag a) -> TypeingOut (GTransformType (Tag a))
mergeTTaged ty1@(GTransformType c1 from1 to1) ty2@(GTransformType c2 from2 to2) | M.null c2 = do
  theC <- constraints to1 from2
  -- apply the solution of the merge of the type vars and then adjust buffersizes
  joinT (applym theC ty1) (applym theC ty2)
mergeTTaged _ _ = fail "cannot merge with constraint on the right"

joinT :: Tagable a => GTransformType (Tag a) -> GTransformType (Tag a) -> TypeingOut (GTransformType (Tag a))
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

joinT1 :: RecordType a -> RecordType a -> TypeingOut (GTransformType a)
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
    (cTypesOut, fFrom, fTo) <- joinT2 cTypes arg1 arg2 arg3 arg4
    let c1new = zip (map fst c1) (zipWith reJoin (map snd c1) cTypesOut) -- TODO check
    return (GTransformType (M.fromAscList c1new) (zipWith reJoin from1 fFrom) (zipWith reJoin to2 fTo))
    where
      reJoin :: GaiwanBuf a -> (String, Int, Int) -> GaiwanBuf a
      reJoin (GaiwanBuf exp gs) newSize = GaiwanBuf (denorm newSize) gs
joinT1 _ _ = fail "mc2 was non empty"

joinT2 ::
  -- |  constraint acc
  [(String, Int, Int)] ->
  -- | from acc
  [(String, Int, Int)] ->
  -- | from todo
  [(String, Int, Int)] ->
  -- | to todo
  [(String, Int, Int)] ->
  -- | to acc
  [(String, Int, Int)] ->
  TypeingOut ([(String, Int, Int)], [(String, Int, Int)], [(String, Int, Int)])
joinT2 c f [] [] t = return (c, f, t)
joinT2 c f (l1 : lr) (r1 : rr) t = do
  (l, r) <- solveTCnt l1 r1
  fN <- mapM (l True) f
  tN <- mapM (r True) t
  lN <- mapM (l True) lr
  rN <- mapM (r True) rr
  cN <- mapM (l False) c
  joinT2 cN fN lN rN tN
joinT2 _ f _ _ t = fail "incompatible number of buffers"

-- | solve
solveTCnt ::
  (String, Int, Int) ->
  (String, Int, Int) ->
  TypeingOut
    ( Bool -> (String, Int, Int) -> TypeingOut (String, Int, Int),
      Bool -> (String, Int, Int) -> TypeingOut (String, Int, Int) -- (Sizes of the inputs, sizes of the output)
    )
solveTCnt
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified
  (name3, a3, b3) -- -/
    | name2 == name3 && a2 == a3 && b2 == b3 && name2 /= "n" =
      return (const return, const return) -- basically id
solveTCnt
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified
  (name3, a3, b3) -- -/
    | name2 == name3 && b2 == b3 && (a2 == 0 || a3 == 0) && name2 /= "n" -- TODO: FIXME REMOVE
      =
      -- we know that the linear factor must be zero (0*x + b = a₃*x + b ⇒ a₃ = 0)
      do
        uniqv <- nextUniqv
        let zeroTransformer = \assertName (name1, a1, b1) ->
              if name1 == name2 || a1 == 0
                then return ("joinedsize" ++ show uniqv, 0, b1)
                else return (name1, a1, b1) -- leave unchanged
        return (zeroTransformer, zeroTransformer)
solveTCnt
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified
  (name3, a3, b3) -- -/
    | name2 == name3 && name2 /= "n" -- TODO: FIXME REMOVE
      =
      fail $ "oh no " ++ show ((name2, a2, b2), (name3, a3, b3))
solveTCnt
  (name2, a2, b2) -- overlapping sizes, these 2 should be unified
  (name3, a3, b3) -- -/
    =
    do
      -- Solve modulo operation to find value for v
      uniqv <- nextUniqv

      v <-
        if b2 == b3
          then return 0
          else
            maybe
              (fail $ "Could not unify" ++ show (a2, b3 - b2, a3) ++ show (map (\v -> (a2 * v - (b3 - b2)) `mod` a3 == 0) [0 .. (abs a3)]))
              return
              $ find (\v -> (a2 * v - (b3 - b2)) `mod` a3 == 0) [0 .. (abs a3)]
      let leftTransformer = \assertName (name1, a1, b1) ->
            if name1 == name2 || a1 == 0
              then return ("joinedsize" ++ show uniqv, a1 * u, b1 + a1 * v)
              else return (name1, a1, b1) -- leave unchanged
      let rightTransformer = \assertName (name4, a4, b4) ->
            if name4 == name3 || a4 == 0
              then return ("joinedsize" ++ show uniqv, a4 * div (a2 * u) a3, b4 + a4 * div (a2 * v + b2 - b3) a3)
              else return (name4, a4, b4) -- leave unchanged
      return (leftTransformer, rightTransformer)
    where
      n = Var name2 False
      u = abs $ div a3 (gcd a2 a3)

normBufSize :: GaiwanBuf a -> TypeingOut (String, Int, Int)
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
renameb v = apply $ \name -> TVar (v, name)

apply :: (a -> GShape b) -> GaiwanBuf a -> GaiwanBuf b
apply f (GaiwanBuf exp gs) = GaiwanBuf exp (applys f gs)

-- apply for shapes only
applys :: (a -> GShape b) -> GShape a -> GShape b
applys f (TVar a) = f a
applys f GaiwanInt = GaiwanInt
applys f (GaiwanTuple a) = GaiwanTuple $ map (applys f) a

unrename :: Tagable a => GTransformType (Tag a) -> GTransformType a
unrename (GTransformType c gbs gb) = GTransformType (M.map unrenameb c) (map unrenameb gbs) (map unrenameb gb)

unrenameb :: Tagable a => GaiwanBuf (Tag a) -> GaiwanBuf a
unrenameb = apply $ \tagedname -> TVar $ untagify tagedname

-- | Unify the size of buffers
bufType :: TaggedBuff a -> GShape (Tag a)
bufType (GaiwanBuf exp gs) = gs

-- | Normalize a size expression and get (varname, a, b) = a*varname + b
norm :: Exp -> TypeingOut (String, Int, Int)
norm (Times (Int a2) (Var name2 False)) = return (name2, a2, 0)
norm (Times (Var name2 False) (Int a2)) = return (name2, a2, 0)
norm (Var name2 False) = return (name2, 1, 0)
norm (Plus (Var name2 False) (Int b)) = return (name2, 1, b)
norm (Plus (Times (Var name2 False) (Int a)) (Int b)) = return (name2, a, b)
norm (Plus (Times (Int a) (Var name2 False)) (Int b)) = return (name2, a, b)
norm (Int b) = do
  uniqv <- nextUniqv
  return ("freelen_" ++ show uniqv, 0, b)
norm idk = fail $ "Could not normalize " ++ show idk

denorm :: (String, Int, Int) -> Exp
denorm (name, a, k) = simplifyExp $ Plus (Times (Int a) (Var name False)) (Int k)

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
typeOfBody env a@App {} = fail $ "I don't know this applicaion" ++ show a
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
bufferType1 (Just (ABuf (GaiwanBuf (Int 1) b))) = return $ Just b
bufferType1 (Just (ABuf GaiwanBuf {})) = fail "non buffer type with length one for out of reducer"
bufferType1 (Just _) = fail "non buffer type"
bufferType1 Nothing = return Nothing

-- | Get the type of the elements of a buffer argument
liftMaybeBuff :: (String, Maybe GBufOrShapeDefault) -> TypeingOut (String, GaiwanBufDefault)
liftMaybeBuff (a, Nothing) = fail "No type given"
liftMaybeBuff (a, Just (ABuf buf)) = return (a, buf)
liftMaybeBuff (a, Just (AShape _)) = fail "Got scalar, expeced buffer"
