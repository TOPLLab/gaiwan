{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CodeGen.Pipelining (Pipeline, collectBuffers, convertPls) where

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
collectBuffers p =  undefined


convertPls = undefined

