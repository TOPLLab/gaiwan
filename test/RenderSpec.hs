{-# LANGUAGE QuasiQuotes #-}

module RenderSpec (spec) where

--import CodeGen.Render
import qualified Data.ByteString.Lazy as BS
import Language.Gaiwan
import Language.GaiwanDefs
import Render
import RenderDefs
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ


spec =
  describe "Render" $ do
    it "makes a good model" $ do
        1 `shouldBe` 1
