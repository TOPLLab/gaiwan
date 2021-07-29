{-# LANGUAGE TupleSections #-}

module LanguageSpec (spec) where

import Data.Either
import Language.Gaiwan
import Language.GaiwanDefs
import Language.GaiwanTypes
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

    it "Merges types correctly: n->n # n->n => n->n with type vars" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "a", TVar "a", TVar "b", TVar "b"])))
        (GaiwanArrow (GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "b", TVar "c", TVar "c", TVar "d"])) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))

    it "Merges types correctly: n->n # n->n => n->n" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))

    it "Merges types correctly: n->n+2 # 2n-> n => 2n -> n+1" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt) (GaiwanBuf (Plus (Var "n" False) (Int 1)) GaiwanInt))

    it "Merges types correctly: n->n+2 # 2n+1->n  => 2n+1->n+1" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 1)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 1)) GaiwanInt) (GaiwanBuf (Plus (Var "n" False) (Int 1)) GaiwanInt))

    it "Merges types correctly: 2n->n+2 # 3n+1->n => 6n+4->n+1" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Times (Var "n" False) (Int 2)) GaiwanInt) (GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Plus (Times (Int 6) (Var "n" False)) (Int 4)) GaiwanInt) (GaiwanBuf (Plus (Var "n" False) (Int 1)) GaiwanInt))

    it "Merges types correctly: 2n->9n+1 # 3n+1->n => 2n->3n" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Times (Var "n" False) (Int 2)) GaiwanInt) (GaiwanBuf (Plus (Times (Int 9) (Var "n" False)) (Int 1)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt) (GaiwanBuf (Times (Int 3) (Var "n" False)) GaiwanInt))

    it "Merges types correctly: 2n->9n+4 # 3n+1->n => 2n->3n + 1" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Times (Var "n" False) (Int 2)) GaiwanInt) (GaiwanBuf (Plus (Times (Int 9) (Var "n" False)) (Int 4)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt) (GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt))

    it "Merges types correctly: n->3n+1 # 9n+4 -> n => 3n+1 -> n " $
      mergeT
        (GaiwanArrow (GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt) (GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 9) (Var "n" False)) (Int 4)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))

    it "Merges types correctly: n->11n+2 # n+3->n => n->11n-1" $ -- TODO
      mergeT
        (GaiwanArrow (GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt) (GaiwanBuf (Plus (Times (Int 11) (Var "n" False)) (Int 2)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 1) (Var "n" False)) (Int 3)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Plus (Times (Int 11) (Var "n" False)) (Int (-1))) GaiwanInt))

    it "Merges types correctly: n->11n+2 # 7n+3->n => 7n+2->11n+3" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt) (GaiwanBuf (Plus (Times (Int 11) (Var "n" False)) (Int 2)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 7) (Var "n" False)) (Int 3)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Plus (Times (Int 7) (Var "n" False)) (Int 2)) GaiwanInt) (GaiwanBuf (Plus (Times (Int 11) (Var "n" False)) (Int 3)) GaiwanInt))

    it "Merges types correctly: n->100-n # 200-2n->n => n->n+100" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 100)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 200)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Plus (Var "n" False) (Int 100)) GaiwanInt))

    it "Merges types correctly: n->100-n # 200-2n->n => 2n->n+50" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 100)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int $ -2) (Var "n" False)) (Int 200)) GaiwanInt) (GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt) (GaiwanBuf (Plus (Var "n" False) (Int 50)) GaiwanInt))

    it "Merges types correctly: n->100-n # 2n->n => 2n->50-n" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 100)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 0)) GaiwanInt) (GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt) (GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 50)) GaiwanInt))

    it "Merges types correctly: n->2n+2 # 2n->50-n => n->49-n" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Plus (Times (Int $ 2) (Var "n" False)) (Int 2)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 0)) GaiwanInt) (GaiwanBuf (Plus (Times (Var "n" False) (Int $ -1)) (Int 50)) GaiwanInt))
        `shouldBe` Right (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 49)) GaiwanInt))

    it "Merges types correctly: n->2n+1 # 2n->n => FAIL" $
      mergeT
        (GaiwanArrow (GaiwanBuf (Var "n" False) GaiwanInt) (GaiwanBuf (Times (Var "n" False) (Int 2)) GaiwanInt))
        (GaiwanArrow (GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 1)) GaiwanInt) (GaiwanBuf (Var "n" False) GaiwanInt))
        `shouldSatisfy` isLeft

    -- Handle empty case

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
                [("i", Nothing), ("v", Just (GaiwanBuf (Plus (Times (Int 2) n) (Int 1)) (TVar "B")))]
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
        `shouldBe` Right
          ( TAbstraction
              ( GaiwanArrow
                  (GaiwanBuf (Plus (Times (Int 4) (Var "n" False)) (Int 2)) GaiwanInt)
                  (GaiwanBuf (Var "n" False) GaiwanInt)
              )
              "nameOFanAbstraction"
              ["size"]
              [ TShaper
                  ( GaiwanArrow
                      (GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar "A"))
                      (GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "A", TVar "A"]))
                  )
                  "nameOFaMapper"
                  ["i", "v"]
                  (Tuple [ArrayGet (Var "v" False) (Times (Int 2) (Var "i" False)), ArrayGet (Var "v" False) (Plus (Int 1) (Times (Int 2) (Var "i" False)))]),
                TShaper
                  ( GaiwanArrow
                      (GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 1)) (TVar "B"))
                      (GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "B", TVar "B"]))
                  )
                  "nameOFaMapper"
                  ["i", "v"]
                  (Tuple [ArrayGet (Var "v" False) (Times (Int 2) (Var "i" False)), ArrayGet (Var "v" False) (Plus (Int 1) (Times (Int 2) (Var "i" False)))]),
                TMapper
                  ( GaiwanArrow
                      (GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanTuple [GaiwanInt, GaiwanInt], GaiwanTuple [GaiwanInt, GaiwanInt]]))
                      (GaiwanBuf (Var "n" False) GaiwanInt)
                  )
                  "nameOFaMapper"
                  ["i", "v"]
                  (Plus (Select (Select (Var "v" False) 1) 1) (Select (Select (Var "v" False) 1) 0))
              ]
          )

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
