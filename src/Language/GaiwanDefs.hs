module Language.GaiwanDefs
  ( Exp (..),
    Instr (..),
    subst,
    substMult,
    substGPUBuffers,
    simplifyExp,
    simpleSubstMult,
    substArrayGet,
    simpleSubst,
  )
where

import Code.Definitions
import Data.Maybe

data Instr
  = IApp String Bool [Exp]
  | Loop Exp String [Instr]
  | LetB String [Instr] [Instr]
  | Return [String]
  deriving (Show, Eq)

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

-- Substitute
substMult :: [(Exp, Exp)] -> Exp -> Exp
substMult [] c = c
substMult kv c = mapExp (_subst kv) c
  where
    _subst kv (Let varName val rest) = Just $ Let varName (mapExp (_subst kv) val) (mapExp (_subst (delKey (Var varName False) kv)) rest)
    _subst kv c = lookup c kv

substArrayGet :: String -> (Exp -> Exp) -> Exp -> Exp
substArrayGet varname trans = simplifyExp . mapExp doSubstArrayGet
  where
    doSubstArrayGet :: Exp -> Maybe Exp
    doSubstArrayGet (ArrayGet (Var name False) index) | name == varname = Just $ trans index
    doSubstArrayGet (ArrayGet Var {} index) = Nothing
    doSubstArrayGet (ArrayGet _ index) = error "TODO: non var ArrayGet"
    doSubstArrayGet _ = Nothing

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
-- The select-Tuple combo brings down the number of selects in bitonic sort from 4210 to 314
simplifyExp :: Exp -> Exp
simplifyExp e =
  let rec = mapExp _simplifyExp e
   in if rec == e then e else simplifyExp rec
  where
    _simplifyExp e@(Select (Let s exp exp') i) = Just $ Let s exp (Select exp' i)
    _simplifyExp e@(Select (Tuple exps) i) = Just $ exps !! i -- TODO doe we need to check the lenght here? It is already typechecked normally...
    -- _simplifyExp e@(Select (If exp exp' exp2) i) = _
    _simplifyExp (Let varname a@(Int _) b) = Just $ subst (Var varname False) a b
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
    _mapExp e@(Let s val exp) = Let s (f val) (f exp)
    _mapExp e@(Plus a b) = Plus (f a) (f b)
    _mapExp e@(Minus a b) = Minus (f a) (f b)
    _mapExp e@(App name builtin exps) = App name builtin (map f exps)
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
    _mapExp e@GPUBufferGet {} = error "do not use " -- TODO: remove
    _mapExp e@Int {} = e
    _mapExp e@Var {} = e
