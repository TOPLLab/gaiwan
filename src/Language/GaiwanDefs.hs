{-# LANGUAGE FlexibleInstances #-}

module Language.GaiwanDefs
  ( Exp (..),
    Instr (..),
    Constraints,
    TypeingOut,
    nextUniqv,
    GaiwanBufSize (..),
    GaiwanBuf (..),
    GTransformType (..),
    GShape (..),
    GAbsType (..),
    Stmt (..),
    ArgList (..),
    Abstraction (..),
    GBufOrShape (..),
    Program (..),
    subst,
    substMult,
    substGPUBuffers,
    simplifyExp,
    simpleSubstMult,
    mapExp,
    VarSpecifier,
    GExp (..),
    substArrayGet,
    simpleSubst,
    Void (..),
    Freshable (..),
  )
where

import Control.Monad.State.Lazy
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace

data Instr
  = IApp String Bool [Exp]
  | Loop Exp String [Instr]
  | LetB String [Instr] [Instr]
  | Return [String]
  deriving (Show, Eq)

data GExp a
  = Let String (GExp a) (GExp a)
  | Plus (GExp a) (GExp a)
  | Minus (GExp a) (GExp a)
  | Modulo (GExp a) (GExp a)
  | Times (GExp a) (GExp a)
  | Pow (GExp a) (GExp a)
  | Div (GExp a) (GExp a)
  | Int Int
  | Tuple [(GExp a)]
  | Select (GExp a) Int
  | Var String Bool
  | Negate (GExp a)
  | ArrayGet (GExp a) (GExp a)
  | GPUBufferGet a (GExp a) -- Not expressable in syntax (if a = Void)
  | If (GExp a) (GExp a) (GExp a)
  | IsEq (GExp a) (GExp a)
  | IsGreater (GExp a) (GExp a)
  deriving (Show, Eq, Ord)

data Void

instance Eq Void where
  (==) _ _ = False

instance Ord Void where
  (<=) _ _ = False

instance Show Void where
  show _ = error "calling show on nonexisent value"

type Exp = GExp Void -- Exp without GPUBuffer Get

class Eq a => Freshable a where
  fresh :: TypeingOut a

data Program a
  = Prog [Abstraction a] [Instr]
  deriving (Show, Eq)

data Abstraction a = Abstraction (Maybe (GBufOrShape a)) String (ArgList a) [Stmt a]
  deriving (Show, Eq)

type ArgList a = [(String, Maybe (GBufOrShape a))]

type TypeingOut = StateT Int (Either String)

nextUniqv :: TypeingOut Int
nextUniqv = do
  uniqv <- get
  put (uniqv + 1)
  return uniqv

instance MonadFail (Either String) where
  fail = Left

type Constraints a = M.Map String (GaiwanBuf a)

data GaiwanBufSize a = GaiwanBufSize a Int Int
  deriving (Show, Eq)

data GShape a
  = GaiwanInt -- TODO: float
  | GaiwanTuple [GShape a]
  | TVar a
  deriving (Show, Eq, Ord, Read)

data GaiwanBuf a = GaiwanBuf (GaiwanBufSize a) (GShape a)
  deriving (Show, Eq)

data GTransformType a = GTransformType (Constraints a) [GaiwanBuf a] [GaiwanBuf a]
  deriving (Show, Eq)

-- Type of a abstraction (scalar vars ,  transformtype if called)
data GAbsType a = GaiwanArrow [GShape a] (GTransformType a)
  deriving (Show, Eq)

data GBufOrShape a = ABuf (GaiwanBuf a) | AShape (GShape a) deriving (Show, Eq)

data Stmt a
  = Mapper (Maybe (GBufOrShape a)) String (ArgList a) Exp
  | Reducer (Maybe (GBufOrShape a)) String (ArgList a) Exp Exp
  | Shaper (Maybe (GBufOrShape a)) String (ArgList a) Exp
  deriving (Show, Eq)

type VarSpecifier = (String, Bool)

simpleSubst :: (Eq a, Ord a, Show a) => VarSpecifier -> GExp a -> GExp a -> GExp a
simpleSubst from to c = simplifyExp $ subst from to c

simpleSubstMult :: (Eq a, Ord a, Show a) => [(VarSpecifier, (GExp a))] -> (GExp a) -> (GExp a)
simpleSubstMult mapping to = simplifyExp $ substMult mapping to

-- Substiute a for b in c
subst :: VarSpecifier -> (GExp a) -> (GExp a) -> (GExp a)
subst from to = substMult [(from, to)]

-- Substitute
substMult :: [(VarSpecifier, (GExp a))] -> (GExp a) -> (GExp a)
substMult [] c = c
substMult kv c = mapExp (_substM kv) c
  where
    _substM kvA (Let varName val rest) = Just $ Let varName (mapExp (_substM kvA) val) (mapExp (_substM (delKey (varName, False) kvA)) rest)
    _substM kvA (Var a b) = lookup (a, b) kvA
    _substM kvA (GPUBufferGet buf exp) = Just $ GPUBufferGet buf (substMult kv exp)
    _substM _ _ = Nothing

substArrayGet :: (Eq a, Show a, Ord a) => String -> (GExp a -> GExp a) -> GExp a -> GExp a
substArrayGet varname trans = simplifyExp . mapExp doSubstArrayGet
  where
    doSubstArrayGet (ArrayGet (Var name False) index) | name == varname = Just $ trans index
    doSubstArrayGet (ArrayGet Var {} index) = Nothing
    doSubstArrayGet (ArrayGet _ index) = error "Non var ArrayGets are illegal"
    doSubstArrayGet (GPUBufferGet buf exp) = Just $ GPUBufferGet buf exp -- keeps GPUBufferGets
    doSubstArrayGet x = Nothing

substGPUBuffers :: Eq a => [(GaiwanBuf a, GaiwanBuf a)] -> GExp (GaiwanBuf a) -> GExp (GaiwanBuf a)
substGPUBuffers [] c = c
substGPUBuffers kv c = mapExp (_subst kv) c
  where
    -- loop: remove var
    _subst kv c@(GPUBufferGet buf exp) = do
      otherBuf <- lookup buf kv
      return $ GPUBufferGet otherBuf (substGPUBuffers kv exp)
    _subst kv c = Nothing

data SimplifyData a
  = SimplifyData
      Int -- Let index
      [GExp a] -- common expressions

-- | Simplify till fixed point :TODO CONTINUE HERE
-- The select-Tuple combo brings down the number of selects in bitonic sort from 4210 to 314
simplifyExp :: (Eq a, Ord a, Show a) => GExp a -> (GExp a)
simplifyExp e = fst $ runState (simplifyExpS e) (SimplifyData (findBestLiftedId e 1) [])

-- find lowest id to start lets with
findBestLiftedId :: GExp a -> Int -> Int
findBestLiftedId e i =
  let iname = "lifted" ++ (show i)
   in if isVarUsed iname e then (findBestLiftedId e (i + 10)) else i

isVarUsed iname e = not $ null $ findVarUsages iname e

findVarUsages iname =
  findMatches
    ( \x -> case x of
        (Let s _ _) -> s == iname
        (Var s b) -> s == iname
        _ -> False
    )
    (const True)

findMatches :: (GExp a -> Bool) -> (String -> Bool) -> GExp a -> [GExp a]
findMatches ePred letPred e = if ePred e then (e : (findMatchesRec e)) else (findMatchesRec e)
  where
    findMatchesRec (Let s ge ge') | letPred s = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')
    findMatchesRec (Let s ge ge') = []
    findMatchesRec (Plus ge ge') = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')
    findMatchesRec (Minus ge ge') = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')
    findMatchesRec (Modulo ge ge') = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')
    findMatchesRec (Times ge ge') = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')
    findMatchesRec (Pow ge ge') = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')
    findMatchesRec (Div ge ge') = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')
    findMatchesRec (Int n) = []
    findMatchesRec (Tuple ges) = concatMap (findMatches ePred letPred) ges
    findMatchesRec (Select ge n) = (findMatches ePred letPred ge)
    findMatchesRec (Var s b) = []
    findMatchesRec (Negate ge) = (findMatches ePred letPred ge)
    findMatchesRec (ArrayGet ge ge') = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')
    findMatchesRec (GPUBufferGet a ge) = (findMatches ePred letPred ge)
    findMatchesRec (If ge ge' ge2) = concatMap (findMatches ePred letPred) [ge, ge', ge2]
    findMatchesRec (IsEq ge ge') = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')
    findMatchesRec (IsGreater ge ge') = (findMatches ePred letPred ge) ++ (findMatches ePred letPred ge')

simplifyExpS :: (Eq a, Ord a, Show a) => GExp a -> State (SimplifyData a) (GExp a)
simplifyExpS e = do
  let result = mapExp _simplifyExp e
  common <-
    mapM
      ( \x -> do
          SimplifyData i e <- get
          put $ SimplifyData (i + 1) e
          return (x, "lifted" ++ (show i))
      )
      $ listCommonSubExpressions result
  let recSubExpLifted = foldr (\(cExp, name) exp -> Let name cExp exp) (mapExp (substCommon (M.fromList $ common)) result) common
  traceM $ show recSubExpLifted
  if recSubExpLifted == e
    then return e
    else simplifyExpS recSubExpLifted
  where
    doRec esub = mapExp _simplifyExp esub

    substCommon common e | M.member e common = Just $ Var (fromJust $ M.lookup e common) False
--    substCommon common (Let varname _ _) = substCommon (M.filter (\x -> x != varname) common) TODODODODOOD
    substCommon common e@(GPUBufferGet {}) = Just $ e
    substCommon common _ = Nothing

    _simplifyExp e@(Select (Let s exp exp') i) = Just $ Let s exp (Select exp' i)
    _simplifyExp e@(Select (Tuple exps) i) = Just $ exps !! i -- TODO do we need to check the lenght here? It is already typechecked normally...
    _simplifyExp e@(Select (If exp exp' exp2) i) = Just $ If exp (Select exp' i) (Select exp2 i)
    _simplifyExp (Let varname a@(Int _) b) = Just $ subst (varname, False) a b
    _simplifyExp (Let varname a@(Var {}) b) = Just $ subst (varname, False) a b
    _simplifyExp (Plus (Int a) (Int b)) = Just $ Int $ a + b
    _simplifyExp (Plus (Int 0) b) = Just $ doRec b
    _simplifyExp (Plus a (Int 0)) = Just $ doRec a
    _simplifyExp (Minus (Int a) (Int b)) = Just $ Int $ a - b
    _simplifyExp (Times (Int a) (Int b)) = Just $ Int $ a * b
    _simplifyExp (Times (Int 0) _) = Just $ Int 0
    _simplifyExp (Times (Int 1) x) = Just x
    _simplifyExp (Times x (Int 1)) = Just x
    _simplifyExp (Times _ (Int 0)) = Just $ Int 0
    _simplifyExp (Div (Int a) (Int b)) = Just $ Int $ div a b
    _simplifyExp (Div a (Int 1)) = Just a
    _simplifyExp (Modulo _ (Int 1)) = Just $ Int 0
    -- _simplifyExp (Modulo (Int 0) _) = Just $ Int 0 -- not always correct !!
    _simplifyExp (Modulo (Int a) (Int b)) = Just $ Int $ mod a b
    _simplifyExp (Pow (Int a) (Int b)) = Just $ Int $ a ^ b
    _simplifyExp (GPUBufferGet b index) = Just $ GPUBufferGet b (doRec index)
    _simplifyExp e = Nothing

findCommonPlusOne e = do
  x <- findCommon e
  return $ x + 1

findCommonPlusOneTwo e1 e2 = do
  x <- findCommon e1
  y <- findCommon e2
  return $ x + y + 1

findCommonPlusOneThree e1 e2 e3 = do
  x <- findCommon e1
  y <- findCommon e2
  z <- findCommon e3
  return $ x + y + z + 1

insertIfGood :: Ord a => Int -> GExp a -> State (M.Map (GExp a) Int) ()
insertIfGood c e | (c < 4) && (c > 1) = do
  m <- get
  let x = M.lookup e m
  put $ case x of
    Nothing -> M.insert e 1 m
    (Just n) -> M.insert e (n + 1) m
insertIfGood c e = return ()

listCommonSubExpressions :: (Eq a, Ord a, Show a) => GExp a -> [GExp a]
listCommonSubExpressions e =
  let (_, m) = (runState (findCommon e) M.empty)
   in map fst $
        (\common -> trace ("common: " ++ show common) common) $
          take 5 $
            L.sortOn (\(_, count) -> -count) (L.filter (\(_, count) -> count > 3) $ M.toList m)

findCommon :: (Eq a, Ord a, Show a) => GExp a -> State (M.Map (GExp a) Int) Int
findCommon e = do
  complexity <- findCommon_ e
  insertIfGood complexity e
  return complexity

findCommon_ (Let varname ge ge') = do
  m <- get
  let (conflicted, safe) = L.partition (\(e,_)->isVarUsed varname e) $ M.toList m
  put $ M.fromList safe
  findCommonPlusOneTwo ge ge' -- don't care about compexity returned
  new_m <- get
  let (new_conflicted, new_safe) = L.partition (\(e,_)->isVarUsed varname e) $ M.toList new_m
  put $ M.fromList (new_safe ++ safe)
  return $ trace ("conflicts: " ++ show new_conflicted) 100
findCommon_ (Plus ge ge') = findCommonPlusOneTwo ge ge'
findCommon_ (Minus ge ge') = findCommonPlusOneTwo ge ge'
findCommon_ (Modulo ge ge') = findCommonPlusOneTwo ge ge'
findCommon_ (Times ge ge') = findCommonPlusOneTwo ge ge'
findCommon_ (Pow ge ge') = findCommonPlusOneTwo ge ge'
findCommon_ (Div ge ge') = findCommonPlusOneTwo ge ge'
findCommon_ (Int n) = return 1
findCommon_ (Tuple ges) = return 100
findCommon_ (Select ge n) = return 100
findCommon_ (Var s b) = return 1
findCommon_ (Negate ge) = findCommon ge
findCommon_ (ArrayGet ge ge') = findCommonPlusOne ge'
findCommon_ (GPUBufferGet a ge) = findCommonPlusOne ge
findCommon_ (If ge ge' ge2) = findCommonPlusOneThree ge ge' ge2
findCommon_ (IsEq ge ge') = findCommonPlusOneTwo ge ge'
findCommon_ (IsGreater ge ge') = findCommonPlusOneTwo ge ge'

delKey :: (Eq k) => k -> [(k, b)] -> [(k, b)]
delKey key = filter filterFun
  where
    filterFun (k, _) | k == key = False
    filterFun (k, _) = True

-- Recursively perform an function on an expression.
-- If the given function return
--    - None: The call is retried on the components
--    - Just x: mapExp returns x (the functions should take care of recursion)
mapExp :: (GExp a -> Maybe (GExp b)) -> GExp a -> GExp b
mapExp fOrig e = fromMaybe (_mapExp e) (fOrig e)
  where
    f = mapExp fOrig
    _mapExp e@(Let s val exp) = Let s (f val) (f exp)
    _mapExp e@(Plus a b) = Plus (f a) (f b)
    _mapExp e@(Minus a b) = Minus (f a) (f b)
    _mapExp e@(Modulo a b) = Modulo (f a) (f b)
    _mapExp e@(Times a b) = Times (f a) (f b)
    _mapExp e@(Pow a b) = Pow (f a) (f b)
    _mapExp e@(Div a b) = Div (f a) (f b)
    _mapExp e@(Negate a) = Negate (f a)
    _mapExp e@(ArrayGet a b) = ArrayGet (f a) (f b)
    _mapExp e@(If a b c) = If (f a) (f b) (f c)
    _mapExp e@(IsEq a b) = IsEq (f a) (f b)
    _mapExp e@(IsGreater a b) = IsGreater (f a) (f b)
    _mapExp e@(Tuple es) = Tuple (map f es)
    _mapExp e@(Select a b) = Select (f a) b
    _mapExp e@(GPUBufferGet {}) = error "Cannot mapExp a GPUBufferGet"
    _mapExp (Int x) = Int x
    _mapExp (Var a b) = Var a b
