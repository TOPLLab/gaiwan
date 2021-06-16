{-# LANGUAGE TupleSections #-}

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

demoNameAndContents fname = (fname,) <$> readFile ("demo" </> fname)

n = Var "n" False

v = Var "v" False

spec = do
  describe "Language.GaiwanDefs (type)" $ do
    it "notice missing var" $
      toTypedSmt (Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just GaiwanInt)] n) `shouldSatisfy` isLeft

    it "types a simple mapper " $
      toTypedSmt (Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just GaiwanInt)] v)
        `shouldBe` Right
          ( TMapper
              ( GaiwanArrow
                  (GaiwanBuf n GaiwanInt)
                  (GaiwanBuf n GaiwanInt)
              )
              "nameOFaMapper"
              ["i", "v"]
              v
          )
    it "types a simple shaper " $
      toTypedSmt (Shaper (Just (GaiwanBuf n GaiwanInt)) "nameOFaMapper" [("i", Nothing), ("v", Just (GaiwanBuf n GaiwanInt))] (ArrayGet v (Var "i" False)))
        `shouldBe` Right
          ( TShaper
              (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
              "nameOFaMapper"
              ["i", "v"]
              (ArrayGet (Var "v" False) (Var "i" False))
          )

    it "types a simple reducer " $
      toTypedSmt (Reducer (Just (GaiwanBuf (Int 1) GaiwanInt)) "nameOFaMapper" [("i", Nothing), ("acc", Nothing), ("v", Just GaiwanInt)] (Int 5) (Plus (Var "acc" False) v))
        `shouldBe` Right
          ( TReducer
              (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Int 1) GaiwanInt))
              "nameOFaMapper"
              ["i", "acc", "v"]
              (Int 5)
              (Plus (Var "acc" False) (Var "v" False))
          )

    it "types a simple an empty abstraction" $
      toTypedSmt (Abstraction Nothing "nameOFanAbstraction" [("size", Just GaiwanInt)] [])
        `shouldSatisfy` isLeft

    it "types a simple mapper in an abstraction" $
      toTypedSmt
        ( Abstraction
            Nothing
            "nameOFanAbstraction"
            [("size", Just GaiwanInt)]
            [Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just GaiwanInt)] (Plus (Var "size" False) v)]
        )
        `shouldBe` Right
          ( TAbstraction
              (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
              "nameOFanAbstraction"
              ["size"]
              [TMapper (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt)) "nameOFaMapper" ["i", "v"] (Plus (Var "size" False) (Var "v" False))]
          )
    it "types a two mapper in an abstraction" $
      toTypedSmt
        ( Abstraction
            Nothing
            "nameOFanAbstraction"
            [("size", Just GaiwanInt)]
            [ Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just GaiwanInt)] (Plus (Var "size" False) v),
              Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just GaiwanInt)] (Plus (Var "size" False) v)
            ]
        )
        `shouldBe` Right
          ( TAbstraction
              (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
              "nameOFanAbstraction"
              ["size"]
              [ TMapper (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt)) "nameOFaMapper" ["i", "v"] (Plus (Var "size" False) (Var "v" False)),
                TMapper (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt)) "nameOFaMapper" ["i", "v"] (Plus (Var "size" False) (Var "v" False))
              ]
          )

    it "types a two mapper in an abstraction with var" $
      toTypedSmt
        ( Abstraction
            Nothing
            "nameOFanAbstraction"
            [("size", Just GaiwanInt)]
            [ Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just GaiwanInt)] (Plus (Var "size" False) v),
              Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (TVar "Out"))] v,
              Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (TVar "Out"))] (Tuple [v, v])
            ]
        )
        `shouldBe` Right
          ( TAbstraction
              (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanInt, GaiwanInt])))
              "nameOFanAbstraction"
              ["size"]
              [ TMapper
                  (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
                  "nameOFaMapper"
                  ["i", "v"]
                  (Plus (Var "size" False) (Var "v" False)),
                TMapper
                  (GaiwanArrow (GaiwanBuf (Var "n" False) (TVar "Out")) (GaiwanBuf (Var "n" False) (TVar "Out")))
                  "nameOFaMapper"
                  ["i", "v"]
                  (Var "v" False),
                TMapper
                  (GaiwanArrow (GaiwanBuf (Var "n" False) (TVar "Out")) (GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "Out", TVar "Out"])))
                  "nameOFaMapper"
                  ["i", "v"]
                  (Tuple [Var "v" False, Var "v" False])
              ]
          )

    it "types a two mapper in an abstraction with var" $
      toTypedSmt
        ( Abstraction
            Nothing
            "nameOFanAbstraction"
            [("size", Just GaiwanInt)]
            [ Shaper
                (Just (GaiwanBuf n (GaiwanTuple [TVar "A", TVar "A"])))
                "nameOFaMapper"
                [ ("i", Nothing),
                  ("v", Just (GaiwanBuf (Times (Int 2) n) (TVar "A")))
                ]
                $ Tuple
                  [ ArrayGet v (Times (Int 2) (Var "i" False)),
                    ArrayGet v (Plus (Int 1) (Times (Int 2) (Var "i" False)))
                  ],
              Shaper
                (Just (GaiwanBuf n (GaiwanTuple [TVar "B", TVar "B"])))
                "nameOFaMapper"
                [("i", Nothing), ("v", Just (GaiwanBuf (Times (Int 2) n) (TVar "B")))]
                $ Tuple
                  [ ArrayGet v (Times (Int 2) (Var "i" False)),
                    ArrayGet v (Plus (Int 1) (Times (Int 2) (Var "i" False)))
                  ],
              Mapper
                Nothing
                "nameOFaMapper"
                [ ("i", Nothing),
                  ("v", Just (GaiwanTuple [GaiwanTuple [GaiwanInt, GaiwanInt], GaiwanTuple [GaiwanInt, GaiwanInt]]))
                ]
                (Plus (Select (Select v 1) 1) (Select (Select v 1) 0))
            ]
        )
        `shouldBe` Left ""

  describe "Language.Gaiwan (parser): check if all demos parse" $ do
    files <- runIO $ listDirectory "demo"
    ffiles <- runIO $ mapM demoNameAndContents files
    mapM_
      ( \(fname, f) ->
          it ("parses " ++ show fname) $
            shouldSatisfy (parseGaiwan f) isRight
      )
      ffiles

  describe "Language.Gaiwan (parser)" $ do
    it "does not parse garbage" $
      parseGaiwan "garbage" `shouldSatisfy` (\(Left _) -> True)

    it "parses a sort.t program correcty" $ do
      d <- readFile "demo/sort.t"
      parseGaiwan d
        `shouldBe` Right
          ( Prog
              [ Abstraction
                  Nothing
                  "llllll"
                  [("round", Just (TVar "int")), ("takePer", Just (TVar "int"))]
                  [Mapper (Just (GaiwanTuple [TVar "int", TVar "int"])) "bitonic_select" [("i", Nothing), ("a", Just (GaiwanTuple [TVar "int", TVar "int"]))] (If (IsGreater (Modulo (Var "i" False) (Pow (Int 2) (Plus (Var "round" False) (Int 1)))) (Pow (Int 2) (Var "round" False))) (If (IsGreater (Select (Var "a" False) 1) (Select (Var "a" False) 2)) (Var "a" False) (Tuple [Select (Var "a" False) 2, Select (Var "a" False) 1])) (If (IsGreater (Select (Var "a" False) 1) (Select (Var "a" False) 2)) (Tuple [Select (Var "a" False) 2, Select (Var "a" False) 1]) (Var "a" False)))],
                Mapper Nothing "randomizer" [("i", Nothing)] (Modulo (Times (Var "i" False) (Int 593)) (Int 1000))
              ]
              (PipedExp [App "generateSeq" True [Int 1, Int 33554432], App "randomizer" False [], Loop (Int 25) "round" [Loop (Plus (Var "round" False) (Int 1)) "step" [App "split" True [Int 2, Pow (Int 2) (Minus (Var "round" False) (Var "step" False))], App "bitonic_select" False [Var "round" False, Plus (Minus (Var "round" False) (Var "step" False)) (Int 1)], App "join" True [Int 2, Pow (Int 2) (Minus (Var "round" False) (Var "step" False))]]]])
          )

-- todo: add test for all demos to see if they are `Right _`
