module Language.GaiwanDefs
  ( Exp (..),
    subst,
    substMult,
    substGPUBuffers,
    simplifyExp,
    simpleSubstMult,
    simpleSubst,
  )
where

import Code.Definitions
import Data.Maybe

data Instr =
   IApp String Bool [Exp]
  | Loop Exp String [Instr]

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
  | Tuple [Exp]
  | Select Exp Int
  | Var String Bool
  | Negate Exp
  | ArrayGet Exp Exp
  | GPUBufferGet GPUBuffer Exp -- Not expressable in syntax
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


-- Substiute a for b in c (with instructions)
substI :: Exp -> Exp -> Instr -> Instr
substI from to = substMultI [(from, to)]


substMultI :: [(Exp,Exp)] -> Instr -> Instr
substMultI kv (Loop exp str instr) = Loop (simplifyExp $ substMult kv exp) str (map (substMultI $ delKey (Var str False) kv) instr)
substMultI kv (IApp str bool expr) = IApp str bool (map (substMult kv) expr)

-- Substitute
substMult :: [(Exp, Exp)] -> Exp -> Exp
substMult [] c = c
substMult kv c = mapExp (_subst kv) c
  where
    _subst kv c = lookup c kv

substGPUBuffers :: [(GPUBuffer, GPUBuffer)] -> Exp -> Exp
substGPUBuffers [] c = c
substGPUBuffers kv c = mapExp (_subst kv) c
  where
    -- loop: remove var
    _subst kv c@(GPUBufferGet buf exp) = do
      otherBuf <- lookup buf kv
      return $ GPUBufferGet otherBuf (substGPUBuffers kv exp)
    _subst kv c = Nothing

-- | Simplify till fixed point :TODO CONTINUE HERE
simplifyExp :: Exp -> Exp
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
    _simplifyExp (Times (Int 1) x) = Just x
    _simplifyExp (Times x (Int 1)) = Just x
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
    _mapExp e@(ArrayGet a b) = ArrayGet (f a) (f b)
    _mapExp e@(Loop cnt name exps) = Loop (f cnt) name (map f exps)
    _mapExp e@(If a b c) = If (f a) (f b) (f c)
    _mapExp e@(IsEq a b) = IsEq (f a) (f b)
    _mapExp e@(IsGreater a b) = IsGreater (f a) (f b)
    _mapExp x = x -- Int, Var
