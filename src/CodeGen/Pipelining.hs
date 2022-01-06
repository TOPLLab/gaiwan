{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CodeGen.Pipelining (Pipeline, collectBuffers, convertPls, makePlan) where

-- The idea is to transform a list of shuffles and maps into a list of maps (containing array)

import Code
import Code.Definitions
import Control.Lens
import Control.Monad
import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Language.GaiwanDefs
import Language.GaiwanTypes

data PipelineStep = PipelineStep
  { _buffer :: Maybe GPUBuffer,
    _expVal :: Exp
  }
  deriving (Show)

makeLenses ''PipelineStep

data Pipeline = Pipeline
  { _shuffle :: Exp, -- Shuffle to apply to the _expValue of the _curExp → Sets the index to write to in the specified buffer?
    _curExp :: PipelineStep, -- TODO: should be a LIST
    _doneExps :: [PipelineStep]
    -- _curSize :: Int -- todo: what is the point of this field?
  }
  deriving (Show)

makeLenses ''Pipeline

-- | Get a list of used GPUBuffers
collectBuffers :: [PipelineStep] -> [GPUBuffer]
collectBuffers p = undefined

convertPls = undefined

type TmpCode a = SCode String a

makePlan :: TypedProgram -> [GPUAction]
makePlan p = snd $ compile $ execCode $ makePlanning p

makePlanning :: TypedProgram -> TmpCode ()
makePlanning (TypedProg actions) = do
  da <- foldM processActions emptyPlan actions
  addHostCode $ Infoz $ show da
  return ()

type PlanData = PipelineStep

-- Substiute a for b in c (with instructions)
substTI :: Exp -> Exp -> TypedInstr -> TypedInstr
substTI from to = substMultTI [(from, to)]

--
-- Don't map the ts of app
substMultTI :: [(Exp, Exp)] -> TypedInstr -> TypedInstr
substMultTI kv (TLoop st cnt varname instrs) = TLoop st (simplifyExp $ substMult kv cnt) varname (map (substMultTI newKv) instrs)
  where
    newKv = filter (\(k, _) -> k /= Var varname False) kv
substMultTI kv (TIApp t ts args) = TIApp t ts (map (simpleSubstMult kv) args)

emptyPlan :: PlanData
emptyPlan =
  PipelineStep
    { _buffer = Nothing,
      _expVal = Int 1
    }

-- TODO check arg length at typechecking

processActions :: PlanData -> TypedInstr -> TmpCode PlanData
processActions pd (TIApp zz (TAbstraction t _ argnames body) argvalues) | length argnames == length argvalues =
  do
    let kv = zip argnames argvalues
    let appliedBody = map (substMultStmt kv) body
    foldM processApplicationD pd appliedBody
processActions foldData (TLoop _ (Int cnt) varname steps) =
  foldM
    processActions
    foldData
    $ concatMap (\i -> map (substTI (Var varname False) (Int i)) steps) [0 .. (cnt -1)]
processActions _ e = error $ "help " ++ show e

substMultStmt :: [(String, Exp)] -> TypedTransform -> TypedTransform
substMultStmt kv (TShaper t name args exp) = TShaper t name args (substExcept kv args exp)
substMultStmt kv (TMapper t name args exp) = TMapper t name args (substExcept kv args exp)
substMultStmt kv (TReducer t name args init exp) = TReducer t name args (substExcept kv args init) (substExcept kv args exp)

substExcept kv args = simpleSubstMult (kvExcept args kv)

kvExcept args kv = map (\(k, vv) -> (Var k False, vv)) $ kvExceptBase args kv

kvExceptBase :: Eq a => [a] -> [(a, b)] -> [(a, b)]
kvExceptBase args = filter (\(k, _) -> k `notElem` args)

theI = Var "i" True

substByTheI x = simpleSubst (Var x False) theI

processApplication :: PlanData -> TypedTransform -> TmpCode PlanData
processApplication pd (TShaper t name [iname] exp) = do
  --addHostCode $ Infoz $ "add call to shaper " ++ name ++ "(XXX)"
  return $ pd & expVal .~ substByTheI iname exp
processApplication pd (TShaper t name [iname, dname] exp) = do
  -- TODO more than one buffer
  --addHostCode $ Infoz $ "add call to shaper " ++ name ++ "("++intercalate "," [iname,dname]++")"
  return $ pd & expVal %~ \old -> substByTheI iname $ substArrayGet dname (\index -> simpleSubst theI index old) exp
processApplication pd (TMapper t name [iname, dname] exp) = do
  --addHostCode $ Infoz $ "add call to  " ++ name ++ "()"
  return $ pd & expVal %~ \old -> substByTheI iname (simpleSubst (Var dname False) old exp)
processApplication _ a = error $ show a

--processApplication pd (TAbstraction t _ [] body) =
--  foldM processApplicationD pd body

processApplicationD a b = do
  x <- processApplication a b
  -- addHostCode $ Infoz $ show x
  return x
