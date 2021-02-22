--import Control.Exception (evaluate)

import Data.Char
import Language.GaiwanDefs
import Lib (convert)
import Test.Hspec
import Test.QuickCheck
import System.Timeout

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

convertT prog = timeout 100000 $ convert prog

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

    it "Correctly simplifies stuff" $ do
      mapM_
        (\(a,b) -> (simplifyExp a) `shouldBe` b)
        [
          (Int 67, Int 67),
          (Times (Int 2) (Int 25), Int 50),
          (Times (Int 2) (Plus (Int 13) (Int 12)), Int 50),
          (Times (Int 10) (Minus (Int 14) (Int 9)), Int 50)
        ]

  describe "Lib.convert (integration tests)" $ do
    it "computes in the right order" $ do
      convertT programSimple `shouldReturn` Just [[1, 1, 1], [2, 2, 2], [3, 3, 3], [4, 4, 4]]

    it "computes the right value for an example program" $ do
      convertT program
        `shouldReturn` Just [ [-1, -2, -3, -4, -5, -6, -7, -8, -9, -10],
                         [-3, -5, -7, -9, -11, -13, -15, -17, -19, -21]
                       ]

    it "computes the right value for an example program with nested for" $ do
      convertT program1Unrolled `shouldReturn` Just expectedLoopReturn

    it "computes the right value for an example program with nested for" $ do
      convertT program1 `shouldReturn` Just expectedLoopReturn

    it "SplitJoin 2 buf offset 1" $ do
      convertT (programSplitJoin 1) `shouldReturn` Just [[0, 11, 20, 13, 40, 15, 60, 17, 80, 19]]

    it "SplitJoin 2 buf offset 2" $ do
      convertT (programSplitJoin 2) `shouldReturn` Just [[0, 10, 12, 13, 40, 50, 16, 17]]

    it "SplitJoin 2 buf offset 3" $ do
      convertT (programSplitJoin 3) `shouldReturn` Just [[0, 10, 20, 13, 14, 15]]

    it "SplitJoin 2 buf offset 0" $ do
      convertT (programSplitJoin 0) `shouldThrow` anyException -- should type error???
    it "Split 2 buf offset 1" $ do
      convertT (programSplit 1) `shouldReturn` Just [[0, 2, 4, 6, 8], [1, 3, 5, 7, 9]]

    it "Split 2 buf offset 2" $ do
      convertT (programSplit 2) `shouldReturn` Just [[0, 1, 4, 5], [2, 3, 6, 7]]

programSimple =
  Prog
    [Mapper "do" ["i", "a", "b", "c", "d"] (Int <$> [1, 2, 3, 4])]
    ( PipedExp
        [ App "generateSeq" True [Int 4, Int 3],
          App "do" False []
        ]
    )

programDefines =
  [ Shuffler "shift" ["index", "A", "Alen", "B", "Blen"] [ArrayGet (Var "A" False) (Modulo (Plus (Var "index" False) (Int 1)) (Var "Alen" False)), ArrayGet (Var "B" False) (Modulo (Minus (Plus (Var "index" False) (Var "Blen" False)) (Int 1)) (Var "Blen" False))],
    Shuffler "swap" ["index", "A", "Alen", "B", "Blen"] [ArrayGet (Var "B" False) (Modulo (Var "index" False) (Var "Blen" False)), ArrayGet (Var "A" False) (Modulo (Var "index" False) (Var "Alen" False))],
    Shuffler "doubler" ["index", "A", "Alen"] [ArrayGet (Var "A" False) (Modulo (Var "index" False) (Var "Alen" False)), ArrayGet (Var "A" False) (Modulo (Var "index" False) (Var "Alen" False))],
    Mapper "haha" ["i", "a", "x", "y"] [Times (Var "a" False) (Var "x" False), Plus (Var "a" False) (Var "y" False)],
    Mapper "inc" ["i", "a", "y"] [Plus (Var "a" False) (Var "y" False)],
    Mapper "id" ["i", "x", "y"] [Var "x" False, Var "y" False],
    Mapper "id1" ["i", "x"] [Var "x" False]
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

expectedLoopReturn = [[20, 21, 22, 23, 24, 25, 26, 27, 28, 29]]

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

programSplit offset =
  Prog
    programDefines
    ( PipedExp
        [ App "generateSeq" True [Int 1, Int 10],
          App "split" True $ Int <$> [2, offset],
          App "id" False []
        ]
    )

programSplitJoin offset =
  let args = Int <$> [2, offset]
   in Prog
        programDefines
        ( PipedExp
            [ App "generateSeq" True [Int 1, Int 10],
              App "split" True args,
              App "haha" False [Int 10],
              App "join" True args
            ]
        )
