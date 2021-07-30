{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CodeGen.Pipelining (Pipeline, collectBuffers, convertPls, makePlan) where

-- The idea is to transform a list of shuffles and maps into a list of maps (containing array)

import Code
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
  { _outBuf :: GPUBuffer,
    _expValue :: Exp
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
makePlanning (TypedProg defs actions) = do
  mapM_ processDefs defs
  foldM_ processActions emptyPlan actions

processDefs :: TypedStmt -> TmpCode ()
processDefs = registerDef

type PlanData = String

emptyPlan :: PlanData
emptyPlan = ""

processActions :: PlanData -> Exp -> TmpCode PlanData
-- processActions _ (App name builtin args) = undefined
processActions foldData (Loop (Int cnt) varname steps) =
  foldM
    processActions
    foldData
    $ concatMap (\i -> map (subst (Var varname False) (Int i)) steps) [0 .. (cnt -1)]
processActions _ e = error $ "help " ++ show e
