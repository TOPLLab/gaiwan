--import Control.Exception (evaluate)

import Data.Char
import Language.GaiwanDefs
import Lib (convert)
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
  oneof $ arbBinOp n m <$> [Plus, Times, Div, Minus]

instance Arbitrary Exp where
  arbitrary = sized arbSizedExp

  shrink (Plus a b) = [a, b]
  shrink (Times a b) = [a, b]
  shrink (Div a b) = [a, b]
  shrink (Minus a b) = [a, b]
  shrink Var {} = []
  shrink (Int 0) = []
  shrink (Int x) = [Int (x -1)]

main :: IO ()
main = hspec $ do
  describe "Language.Gaiwan.subst" $ do
    it "Correctly replaces stuff" $ do
      subst (Var "a" True) (Int 42) (Var "a" True) `shouldBe` Int 42

    it "does nothing when the variable does not occur" $
      property $ \x y z -> subst (Var "no" y) z x `shouldBe` x -- varnames start with var
    it "Correctly replaces stuff" $ do
      subst (Var "a" True) (Int 42) (Plus (Var "a" True) (Int 12)) `shouldBe` Plus (Int 42) (Int 12)

    it "computes the right value for an example program" $ do
      convert program `shouldReturn` [[-3, -5, -7, -9, -11, -13, -15, -17, -19, -21], [-1, -2, -3, -4, -5, -6, -7, -8, -9, -10]]

program =
  Prog
    [ Shuffler "shift" ["index", "A", "Alen", "B", "Blen"] [ArrayGet (Var "A" False) (Modulo (Plus (Var "index" False) (Int 1)) (Var "Alen" False)), ArrayGet (Var "B" False) (Modulo (Minus (Plus (Var "index" False) (Var "Blen" False)) (Int 1)) (Var "Blen" False))],
      Shuffler "swap" ["index", "A", "Alen", "B", "Blen"] [ArrayGet (Var "B" False) (Modulo (Var "index" False) (Var "Blen" False)), ArrayGet (Var "A" False) (Modulo (Var "index" False) (Var "Alen" False))],
      Shuffler "doubler" ["index", "A", "Alen"] [ArrayGet (Var "A" False) (Modulo (Var "index" False) (Var "Alen" False)), ArrayGet (Var "A" False) (Modulo (Var "index" False) (Var "Alen" False))],
      Mapper "haha" ["a", "x", "y"] [Times (Var "a" False) (Var "x" False), Plus (Var "a" False) (Var "y" False)],
      Mapper "id" ["x", "y"] [Var "x" False, Var "y" False]
    ]
    ( PipedExp
        [ App "generateSeq" True [Int 1, Int 10],
          App "doubler" False [],
          Loop
            2
            "i"
            [ App "haha" False [Plus (Var "i" False) (Int 1)],
              App "swap" False [],
              App "shift" False [],
              App "haha" False [Negate (Int 1)]
            ],
          App "id" False []
        ]
    )
