module Specs (main) where

--import Control.Exception (evaluate)
import Language.GaiwanDefs
import Test.Hspec
import Test.QuickCheck

arbBinOp constructor = do
  a <- arbitrary
  b <- arbitrary
  return $ constructor a b

instance Arbitrary Exp where
  arbitrary =
    oneof
      [ Int <$> arbitrary,
        arbBinOp Plus,
        arbBinOp Times,
        arbBinOp Div,
        arbBinOp Minus,
        ( do
            c <- choose (0, 10) :: Gen Int
            k <- arbitrary
            return (Var ("var" ++ show c) k)
        )
      ]

main :: IO ()
main = hspec $ do
  describe "Language.Gaiwan.subst" $ do
    it "Correctly replaces stuff" $ do
        subst (Var "a" True) (Int 42) (Var "a" True) `shouldBe` Int 42

    it "Correctly replaces stuff" $ do
        subst (Var "a" True) (Int 42) (Plus (Var "a" True) (Int 12)) `shouldBe` Plus (Int 42) (Int 12)

    it "does nothing when the variable does not occur" $
      property $ \x y z -> subst (Var "no" y) z x == x -- varnames start with var
