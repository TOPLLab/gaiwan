-- for stmt
{-# LANGUAGE RankNTypes #-}

module Language.GaiwanDefs
  ( Program (..),
    Stmt (..),
    Exp (..),
    subst,
    substMult,
    stmt,
    stmtName,
    simplifyExp,
    simpleSubstMult,
    simpleSubst,
  )
where

import Data.Maybe
import Debug.Trace

data Program
  = Prog [Stmt] Exp
  deriving (Show)

data Stmt
  = Mapper String [String] [Exp]
  | Shuffler String [String] [Exp]
  deriving (Show)

stmt :: forall t. Stmt -> (String -> [String] -> [Exp] -> t) -> t
stmt (Mapper a b c) f = f a b c
stmt (Shuffler a b c) f = f a b c

stmtName :: Stmt -> String
stmtName x = stmt x (\a b c -> a)

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
  | Var String Bool
  | Negate Exp
  | PipedExp [Exp]
  | ArrayGet Exp Exp
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

--
-- Execute till fixed point
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
