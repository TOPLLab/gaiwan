module LanguageSpec (spec) where

import Data.Either
import Language.Gaiwan
import Language.GaiwanDefs
import Lib (convert)
import System.Directory
import System.FilePath
import System.Timeout
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ

demoNameAndContents fname =
  (\v -> (fname, v))
    <$> (readFile $ "demo" </> fname)

spec = do
  describe "Language.Gaiwan (parser): check if all demos parse" $ do
    files <- runIO $ listDirectory "demo"
    ffiles <- runIO $ mapM demoNameAndContents files
    mapM_
      ( \(fname, f) ->
          it ("parses " ++ (show fname)) $ do
            shouldSatisfy (parseGaiwan f) (isRight)
      )
      ffiles

  describe "Language.Gaiwan (parser)" $ do
    it "does not parse garbage" $ do
      parseGaiwan "garbage" `shouldSatisfy` (\(Left _) -> True)

    it "parses a sort.t program correcty" $ do
      d <- readFile "demo/sort.t"
      parseGaiwan d
        `shouldBe` Right
          ( Prog
              [ Mapper "bitonic_select" ["i", "round", "takePer", "a", "b"] [If (IsGreater (Modulo (Var "i" False) (Pow (Int 2) (Plus (Var "round" False) (Int 1)))) (Pow (Int 2) (Var "round" False))) (If (IsGreater (Var "a" False) (Var "b" False)) (Var "a" False) (Var "b" False)) (If (IsGreater (Var "a" False) (Var "b" False)) (Var "b" False) (Var "a" False)), If (IsGreater (Modulo (Var "i" False) (Pow (Int 2) (Plus (Var "round" False) (Int 1)))) (Pow (Int 2) (Var "round" False))) (If (IsGreater (Var "a" False) (Var "b" False)) (Var "b" False) (Var "a" False)) (If (IsGreater (Var "a" False) (Var "b" False)) (Var "a" False) (Var "b" False))],
                Mapper "randomizer" ["i"] [Modulo (Times (Var "i" False) (Int 593)) (Int 1000)]
              ]
              ( PipedExp
                  [ App "generateSeq" True [Int 1, Int (2 ^ 25)],
                    App "randomizer" False [],
                    Loop
                      (Int 25)
                      "round"
                      [ Loop
                          (Plus (Var "round" False) (Int 1))
                          "step"
                          [App "split" True [Int 2, Pow (Int 2) (Minus (Var "round" False) (Var "step" False))], App "bitonic_select" False [Var "round" False, Plus (Minus (Var "round" False) (Var "step" False)) (Int 1)], App "join" True [Int 2, Pow (Int 2) (Minus (Var "round" False) (Var "step" False))]]
                      ]
                  ]
              )
          )

-- todo: add test for all demos to see if they are `Right _`
