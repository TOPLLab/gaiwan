{-# LANGUAGE TupleSections #-}

module UnificationSpec (spec) where

import Language.Gaiwan
import Language.GaiwanDefs
import Test.Hspec
import Test.QuickCheck


spec = do
  describe "Language.GaiwanDefs (type)" $ do
    it "notice missing var" $ do
        (const (TVar "a") (TVar "b")) `shouldSatisfy` (const True)
