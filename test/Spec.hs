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
  oneof $ (arbBinOp n m <$> [Plus, Times, Div, Minus]) ++ [Negate <$> arbSizedExp n]

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

    it "Correctly replaces stuff" $ do
      subst (Var "a" False) (Int 42) (Var "a" False) `shouldBe` Int 42

    it "Correctly replaces stuff" $ do
      subst (Var "a" False) (Int 42) (Loop (Var "a" False) "a" [(Var "a" False)]) `shouldBe` (Loop (Int 42) "a" [(Var "a" False)])

    it "does nothing when the variable does not occur" $
      property $ \x y z -> subst (Var "no" y) z x `shouldBe` x -- varnames start with var
    it "Correctly replaces stuff" $ do
      subst (Var "a" True) (Int 42) (Plus (Var "a" True) (Int 12)) `shouldBe` Plus (Int 42) (Int 12)

    it "computes the right value for an example program" $ do
      convert program `shouldReturn` [[-3, -5, -7, -9, -11, -13, -15, -17, -19, -21], [-1, -2, -3, -4, -5, -6, -7, -8, -9, -10]]

    it "computes the right value for an example program with nested for" $ do
      convert program1Unrolled `shouldReturn` expectedLoopReturn

    it "computes the right value for an example program with nested for" $ do
      convert program1 `shouldReturn` expectedLoopReturn

programDefines =
  [ Shuffler "shift" ["index", "A", "Alen", "B", "Blen"] [ArrayGet (Var "A" False) (Modulo (Plus (Var "index" False) (Int 1)) (Var "Alen" False)), ArrayGet (Var "B" False) (Modulo (Minus (Plus (Var "index" False) (Var "Blen" False)) (Int 1)) (Var "Blen" False))],
    Shuffler "swap" ["index", "A", "Alen", "B", "Blen"] [ArrayGet (Var "B" False) (Modulo (Var "index" False) (Var "Blen" False)), ArrayGet (Var "A" False) (Modulo (Var "index" False) (Var "Alen" False))],
    Shuffler "doubler" ["index", "A", "Alen"] [ArrayGet (Var "A" False) (Modulo (Var "index" False) (Var "Alen" False)), ArrayGet (Var "A" False) (Modulo (Var "index" False) (Var "Alen" False))],
    Mapper "haha" ["i","a", "x", "y"] [Times (Var "a" False) (Var "x" False), Plus (Var "a" False) (Var "y" False)],
    Mapper "inc" ["i","a", "y"] [Plus (Var "a" False) (Var "y" False)],
    Mapper "id" ["i","x", "y"] [Var "x" False, Var "y" False]
  ]

program =
  Prog
    programDefines
    ( PipedExp
        [ App "generateSeq" True [Int 1, Int 10],
          App "doubler" False [],
          Loop
            (Int 2)
            "i"
            [ App "haha" False [Plus (Var "i" False) (Int 1)],
              App "swap" False [],
              App "shift" False [],
              App "haha" False [Negate (Int 1)]
            ]
        ]
    )

expectedLoopReturn = [[20,21,22,23,24,25,26,27,28,29]]

programLoopBody :: Exp -> Exp
programLoopBody i = App "inc" False [i]

program1Unrolled =
  Prog
    programDefines
    ( PipedExp $
        App "generateSeq" True [Int 1, Int 10] :
        concatMap (\i -> map (programLoopBody . Int) [0 .. (i -1)]) [0 .. (6 -1)]
    )

program1 =
  Prog
    programDefines
    ( PipedExp
        [ App "generateSeq" True [Int 1, Int 10],
          Loop
            (Int 6)
            "i"
            [ Loop
                (Var "i" False)
                "j"
                [ programLoopBody (Var "j" False)
                ]
            ],
          App "id" False []
        ]
    )
