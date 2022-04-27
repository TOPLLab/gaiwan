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
import qualified Data.Map as M
import Data.Maybe

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
  deriving (Show, Eq)

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

simpleSubst :: Eq a => VarSpecifier -> GExp a -> GExp a -> GExp a
simpleSubst from to c = simplifyExp $ subst from to c

simpleSubstMult :: Eq a => [(VarSpecifier, (GExp a))] -> (GExp a) -> (GExp a)
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

substArrayGet :: (Eq a, Show a) => String -> (GExp a -> GExp a) -> GExp a -> GExp a
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

-- | Simplify till fixed point :TODO CONTINUE HERE
-- The select-Tuple combo brings down the number of selects in bitonic sort from 4210 to 314
simplifyExp :: Eq a => GExp a -> GExp a
simplifyExp e =
  let rec = mapExp _simplifyExp e
   in if rec == e then e else simplifyExp rec
  where
    _simplifyExp e@(Select (Let s exp exp') i) = Just $ Let s exp (Select exp' i)
    _simplifyExp e@(Select (Tuple exps) i) = Just $ exps !! i -- TODO doe we need to check the lenght here? It is already typechecked normally...
    -- _simplifyExp e@(Select (If exp exp' exp2) i) = _
    _simplifyExp (Let varname a@(Int _) b) = Just $ subst (varname, False) a b
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
    -- _simplifyExp (Modulo (Int 0) _) = Just $ Int 0 -- not always correct !!
    _simplifyExp (Modulo (Int a) (Int b)) = Just $ Int $ mod a b
    _simplifyExp (Pow (Int a) (Int b)) = Just $ Int $ a ^ b
    _simplifyExp (GPUBufferGet b index) = Just $ GPUBufferGet b (simplifyExp index)
    _simplifyExp e = Nothing

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
