{-# LANGUAGE QuasiQuotes #-}

module LanguageSpec (spec) where

import Language.Gaiwan
import Language.GaiwanDefs
import Lib (convert)
import System.Timeout
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ


spec = do
  describe "Language.Gaiwan (parser)" $ do
    it "does not parse garbage" $ do
        parseGaiwan "garbage" `shouldSatisfy` (\(Left _) -> True)

    it "parses a sort.t program correcty" $ do
        d <- readFile "demo/sort.t"
        e <- readFile "test/demo/sort.t.compiled"
        show (parseGaiwan d) `shouldBe` e

    -- todo: add test for all demos to see if they are `Right _`
