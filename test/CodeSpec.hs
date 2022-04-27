{-# LANGUAGE FlexibleInstances #-}

module CodeSpec (spec) where

import Data.Char
import Language.GaiwanDefs
import Lib (convert)
import System.Timeout
import Test.Hspec
import Test.QuickCheck

arbBinOp n m constructor = do
  a <- arbSizedExp n
  b <- arbSizedExp m
  return $ constructor a b

arbSizedExp :: Int -> Gen Exp
arbSizedExp m
  | m < 2 =
    oneof
      [ Int <$> choose (0, 55),
        do
          c <- choose (0, 10) :: Gen Int
          Var ("var" ++ show c) <$> arbitrary
      ]
arbSizedExp m = do
  n <- choose (0, m `div` 2)
  m <- choose (0, m `div` 2)
  oneof $ (arbBinOp n m <$> [Plus, Times, Div, Minus]) ++ [Negate <$> arbSizedExp n]

instance Arbitrary Exp where
  arbitrary = sized arbSizedExp

  shrink (Plus a b) = [a, b]
  shrink (Times a b) = [a, b]
  shrink (Div a b) = [a, b]
  shrink (Minus a b) = [a, b]
  shrink Var {} = []
  shrink (Int 0) = []
  shrink (Int x) = [Int (x - 1)]

convertT prog = timeout 100000 $ convert prog

spec :: Spec
spec = do
  describe "Language.Gaiwan.subst" $ do
    it "Correctly replaces stuff" $ do
      subst ("a", True) (Int 42) ((Var "a" True) :: Exp) `shouldBe` Int 42
