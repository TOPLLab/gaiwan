{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Language.AlphaRename
  ( alphaRenameAbstType,
    alphaRename,
  )
where

import Code.Definitions
import Data.Foldable
import Data.Functor
import qualified Data.Map as M
import Language.GaiwanDefs

-- Alpha rename types
alphaRenameAbstType :: (Ord a, Freshable a, Show a) => GAbsType a -> TypeingOut (GAbsType a)
alphaRenameAbstType (GaiwanArrow argShapes (GTransformType map types1 types2)) | M.size map == 0 =
  do
    lt <- growAlphaLT2 M.empty types1
    argShapes' <- mapM (applyAphaLT lt) argShapes
    types1' <- mapM (applyAphaLT2 lt) types1
    types2' <- mapM (applyAphaLT2 lt) types2
    return (GaiwanArrow argShapes' (GTransformType M.empty types1' types2'))
alphaRenameAbstType (GaiwanArrow argShapes _) = fail "Could not type GaiwanArrow with contraints, these should never have containts"

applyAphaLT2 :: (Ord a, Freshable a, Show a) => M.Map a a -> GaiwanBuf a -> TypeingOut (GaiwanBuf a)
applyAphaLT2 lt (GaiwanBuf exp gs) = do
  gs' <- applyAphaLT lt gs
  return (GaiwanBuf exp gs')

growAlphaLT2 :: (Ord a, Freshable a) => M.Map a a -> [GaiwanBuf a] -> TypeingOut (M.Map a a)
growAlphaLT2 lt [] = return lt
growAlphaLT2 lt ((GaiwanBuf exp gs) : xr) = do
  lt' <- growLTSingle gs lt
  growAlphaLT2 lt' xr

-- | Apply alpha renaming to a shape
-- This will be used storing the value in the TypeingOut monad
-- This will ensure that every time the value is obtained, it is renamed
alphaRename :: (Ord s, Show s, Freshable b) => Abstraction s -> TypeingOut (Abstraction b)
alphaRename (Abstraction m_gbos name args steps) = do
  steps' <- mapM doAlphaRename steps
  lt <- createAlphaLT args
  m_gbos' <- applyAphaLTMaybe lt m_gbos
  args' <- mapM (applyAphaLTArg lt) args
  return $ Abstraction m_gbos' name args' steps'

doAlphaRename :: (Ord s, Show s, Freshable a) => Stmt s -> TypeingOut (Stmt a)
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

applyAphaLTArg :: (Ord s, Show s, Freshable a) => (M.Map s a, M.Map s a) -> (String, Maybe (GBufOrShape s)) -> TypeingOut (String, Maybe (GBufOrShape a))
applyAphaLTArg (lt, ltS) (name, ty) = do
  v <- applyAphaLTMaybe (lt, ltS) ty
  return (name, v)

applyAphaLTMaybe :: (Ord s, Show s, Freshable a) => (M.Map s a, M.Map s a) -> Maybe (GBufOrShape s) -> TypeingOut (Maybe (GBufOrShape a))
applyAphaLTMaybe (lt, ltS) Nothing = return Nothing
applyAphaLTMaybe (lt, ltS) (Just (ABuf (GaiwanBuf exp gs))) = do
  renamedShape <- applyAphaLT lt gs
  renamedSize <- applyAphaLTS ltS exp
  return $ Just $ ABuf (GaiwanBuf renamedSize renamedShape)
applyAphaLTMaybe (lt, ltS) (Just (AShape gs)) = applyAphaLT lt gs <&> Just . AShape

applyAphaLTS :: (Ord s, Show s, Freshable a) => M.Map s a -> GaiwanBufSize s -> TypeingOut (GaiwanBufSize a)
applyAphaLTS lt (GaiwanBufSize varName 0 intercept) = do
  maybe
    ( do
        nn <- fresh
        return $ GaiwanBufSize nn 0 intercept
    )
    (\nn -> return $ GaiwanBufSize nn 0 intercept)
    (M.lookup varName lt)
applyAphaLTS lt (GaiwanBufSize varName slope intercept) =
  maybe
    (fail $ "Could not find " ++ show varName ++ " in applyAphaLTS " ++ show (M.keys lt))
    (\nn -> return $ GaiwanBufSize nn slope intercept)
    (M.lookup varName lt)

applyAphaLT :: (Ord s, Show s, Freshable a) => M.Map s a -> GShape s -> TypeingOut (GShape a)
applyAphaLT lt GaiwanInt = return GaiwanInt
applyAphaLT lt (GaiwanTuple gss) = mapM (applyAphaLT lt) gss <&> GaiwanTuple
applyAphaLT lt (TVar a) = maybe (fail $ "Could not find " ++ show a ++ " in applyAphaLT " ++ (show (M.keys lt))) (return . TVar) (M.lookup a lt)

createAlphaLT :: (Ord s, Freshable a) => ArgList s -> TypeingOut (M.Map s a, M.Map s a)
createAlphaLT = growAlphaLT M.empty M.empty

growAlphaLT :: (Ord s, Freshable a) => M.Map s a -> M.Map s a -> ArgList s -> TypeingOut (M.Map s a, M.Map s a)
growAlphaLT lt ltS [] = return (lt, ltS)
growAlphaLT lt ltS ((s, Nothing) : xr) = growAlphaLT lt ltS xr
growAlphaLT lt ltS ((s, Just bufOrShape) : xr) = do
  lt' <- growLTSingle (collectShape bufOrShape) lt
  ltS' <- growLTSingleVar (collectSizeVar bufOrShape) ltS
  growAlphaLT lt' ltS' xr

growLTSingleVar :: (Ord s, Freshable a) => Maybe s -> M.Map s a -> TypeingOut (M.Map s a)
growLTSingleVar Nothing ltS = return ltS
growLTSingleVar (Just v) ltS | M.member v ltS = return ltS
growLTSingleVar (Just v) ltS = do
  newName <- fresh
  return $ M.insert v newName ltS

collectShape :: GBufOrShape a -> GShape a
collectShape (ABuf (GaiwanBuf _ gs)) = gs
collectShape (AShape gs) = gs

collectSizeVar :: GBufOrShape a -> Maybe a
collectSizeVar (ABuf (GaiwanBuf (GaiwanBufSize name _ _) _)) = Just name
collectSizeVar (AShape gs) = Nothing

growLTSingle :: (Ord s, Freshable a) => GShape s -> M.Map s a -> TypeingOut (M.Map s a)
growLTSingle GaiwanInt lt = return lt
growLTSingle (GaiwanTuple gss) lt = foldrM growLTSingle lt gss
growLTSingle (TVar str) lt | M.member str lt = return lt
growLTSingle (TVar str) lt = do
  newName <- fresh
  return $ M.insert str newName lt
