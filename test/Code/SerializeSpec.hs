{-# LANGUAGE QuasiQuotes #-}

module Code.SerializeSpec (spec) where

import Code.Definitions
import Code.Serialize
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Char
import Language.GaiwanDefs
import Lib (convert)
import System.Timeout
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ

cases =
  [ ("empty", ("", []), [r|["Gaiwan","",[]]|]),
    ("only code", ("Bazinga", []), [r|["Gaiwan","Bazinga",[]]|]),
    ( "simple alloc",
      ("", [AllocBuffer (GPUBuffer (GPUBufferName 42) 17)]),
      [r|["Gaiwan","",[{"alloc":[42,17]}]]|]
    ),
    ( "simple call",
      ("", [CallKernel (KernelName "test") [GPUBuffer (GPUBufferName 12) 10] [] 17]),
      [r|["Gaiwan","",[{"call":"test","buffers":{"used":[[12,10]],"out":[]},"threads":17}]]|]
    ),
    ( "simple read",
      ("", [ReadBuffer (GPUBuffer (GPUBufferName 29) 4)]),
      [r|["Gaiwan","",[{"read":[29,4]}]]|]
    )
  ]

spec = do
  describe "Code.Serialize.serialize" $ do
    mapM_
      ( \(name, a, b) -> it ("serializes " ++ name) $ do
          uncurry serialize a `shouldBe` BSC.pack b
      )
      cases

  describe "Code.Serialize.deserialize" $ do
    mapM_
      ( \(name, a, b) -> it ("serializes " ++ name) $ do
          deserialize (BSC.pack b) `shouldBe` Just a
      )
      cases
    it "checks the magic" $ do
      deserialize (BSC.pack [r|["Tea","",[{"read":[29,4]}]]|]) `shouldBe` Nothing
    it "ignores broken js" $ do
      deserialize (BSC.pack [r|["Gaiwan","",[{"read":[29,4,10000]}]]|]) `shouldBe` Nothing
    it "ignores broken js" $ do
      deserialize (BSC.pack [r|["Gaiwan","",[{"read":{}}]]|]) `shouldBe` Nothing
    it "ignores broken js" $ do
      deserialize (BSC.pack [r|["Gaiwan","",[[]]|]) `shouldBe` Nothing
