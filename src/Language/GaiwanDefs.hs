-- for stmt
{-# LANGUAGE RankNTypes #-}

module Language.GaiwanDefs (Program (..), Stmt (..), Exp (..), subst, substMult, stmt) where

import Data.Maybe

data Program
  = Prog [Stmt] Exp
  deriving (Show)

data Stmt
  = Mapper String [String] [Exp]
  | Shuffler String [String] [Exp]
  | Splitter String [String] [Exp]
  deriving (Show)

stmt :: forall t. Stmt -> (String -> [String] -> [Exp] -> t) -> t
stmt (Mapper a b c) f = f a b c
stmt (Shuffler a b c) f = f a b c
stmt (Splitter a b c) f = f a b c

data Exp
  = Let String Exp Exp
  | Plus Exp Exp
  | Minus Exp Exp
  | App String Bool [Exp]
  | Modulo Exp Exp
  | Times Exp Exp
  | Div Exp Exp
  | Int Int
  | Var String Bool
  | Negate Exp
  | PipedExp [Exp]
  | ArrayGet Exp Exp
  | Loop Exp String [Exp]
  | If Exp Exp Exp
  | IsEq Exp Exp
  deriving (Show, Eq)

-- Substiute a for b in c
subst :: Exp -> Exp -> Exp -> Exp
subst a b = substMult [(a,b)]

-- Substitute arguments
substMult :: [(Exp, Exp)] -> Exp -> Exp
substMult [] c = c
substMult kv c = fromMaybe (_subst c) (lookup c kv)
  where
    -- loop: remove var
    _subst (Loop cntExp varname exps) = Loop (simplifyExp $ recCall cntExp) varname (map (substMult $ delKey (Var varname False) kv) exps)
    -- cases below are the non-special cases
    _subst (Let string exp exp2) = undefined
    _subst (Plus x y) = Plus (recCall x) (recCall y)
    _subst (Minus x y) = Minus (recCall x) (recCall y)
    _subst (App name t args) = App name t (map recCall args)
    _subst (Modulo x y) = Modulo (recCall x) (recCall y)
    _subst (Times x y) = Times (recCall x) (recCall y)
    _subst (Div x y) = Div (recCall x) (recCall y)
    _subst v@Int {} = v
    _subst v@Var {} = v -- actual replacement done in 2nd clause
    _subst (Negate exp) = Negate (recCall exp)
    _subst (PipedExp exps) = PipedExp (map _subst exps)
    _subst (ArrayGet exp idx) = ArrayGet (recCall exp) (recCall idx)
    recCall = substMult kv

simplifyExp (Plus (Int a) (Int b)) = Int $ a + b
simplifyExp (Minus (Int a) (Int b)) = Int $ a - b
simplifyExp (Times (Int a) (Int b)) = Int $ a * b
simplifyExp (Div (Int a) (Int b)) = Int $ div a b
simplifyExp (Modulo (Int a) (Int b)) = Int $ mod a b
simplifyExp e = e

delKey :: Exp -> [(Exp,Exp)] -> [(Exp,Exp)]
delKey key = filter filterFun
  where
    filterFun (k, _) | k == key = False
    filterFun (k, _) = True
