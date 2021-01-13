{-# LANGUAGE RankNTypes #-} -- for stmt

module Language.GaiwanDefs (Program (..), Stmt (..), Exp (..), subst, stmt) where
import Debug.Trace

data Program
  = Prog [Stmt] Exp
  deriving (Show)

data Stmt
  = Mapper String [String] [Exp]
  | Shuffler String [String] [Exp]
  deriving (Show)

stmt :: forall t. Stmt -> (String -> [String] -> [Exp]->t) -> t
stmt (Mapper a b c) f = f a b c
stmt (Shuffler a b c) f = f a b c

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
  | Loop Int String [Exp]
  deriving (Show, Eq)



-- Substiute a for b in c
subst :: Exp -> Exp -> Exp -> Exp
subst a b c | a == c = b
subst (Var a _) b l@(Loop int varname exps) | a == varname = l
subst a@(Var _ _) b c = _subst c
  where -- cases below are the non-special cases
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
    _subst (Loop cnt varname exps) = Loop cnt varname (map recCall exps)
    recCall = subst a b
