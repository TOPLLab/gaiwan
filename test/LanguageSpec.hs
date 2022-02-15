{-# LANGUAGE TupleSections #-}

module LanguageSpec (spec) where

import CodeGen.Pipelining
import Control.Monad.State.Lazy
import Data.Either
import qualified Data.Map as M
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

mergeTStr :: GTransformType String -> GTransformType String -> Either String (GTransformType String)
mergeTStr a b = evalStateT (mergeT a b) 0

joinedSize1 = Var "joinedsize1" False

spec = do
  describe "Language.GaiwanDefs (type)" $ do
    it "notice missing var" $
      toTypedSmtSimple (Abstraction Nothing "fsdf" [] [Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (AShape GaiwanInt))] n]) `shouldSatisfy` isLeft

    it "types a simple mapper " $
      toTypedSmtSimple (Abstraction Nothing "wefw" [] [Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (AShape GaiwanInt))] v])
        `shouldBe` Right (TAbstraction (GaiwanArrow [] (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])) "wefw" [] [TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt]) "nameOFaMapper" ["i", "v"] (Var "v" False)])
    it "types a simple shaper " $
      toTypedSmtSimple (Abstraction Nothing "nameOfAShaper" [] [Shaper (Just (ABuf (GaiwanBuf n GaiwanInt))) "nameOFaMapper" [("i", Nothing), ("v", Just (ABuf (GaiwanBuf n GaiwanInt)))] (ArrayGet v (Var "i" False))])
        `shouldBe` Right (TAbstraction (GaiwanArrow [] (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])) "nameOfAShaper" [] [TShaper (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt]) "nameOFaMapper" ["i", "v"] (ArrayGet (Var "v" False) (Var "i" False))])
    it "types a simple reducer " $
      toTypedSmtSimple (Abstraction Nothing "yolo" [] [Reducer (Just (ABuf (GaiwanBuf (Int 1) GaiwanInt))) "nameOFaMapper" [("i", Nothing), ("acc", Nothing), ("v", Just (AShape GaiwanInt))] (Int 5) (Plus (Var "acc" False) v)])
        `shouldBe` Right (TAbstraction (GaiwanArrow [] (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Int 1) GaiwanInt])) "yolo" [] [TReducer (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Int 1) GaiwanInt]) "nameOFaMapper" ["i", "acc", "v"] (Int 5) (Plus (Var "acc" False) (Var "v" False))])
    it "types a simple an empty abstraction" $
      toTypedSmtSimple (Abstraction Nothing "nameOFanAbstraction" [("size", Just (AShape GaiwanInt))] [])
        `shouldSatisfy` isLeft

    it "types a simple mapper in an abstraction" $
      toTypedSmtSimple
        ( Abstraction
            Nothing
            "nameOFanAbstraction"
            [("size", Just (AShape GaiwanInt))]
            [Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (AShape GaiwanInt))] (Plus (Var "size" False) v)]
        )
        `shouldBe` Right
          ( TAbstraction
              (GaiwanArrow [GaiwanInt] (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt]))
              "nameOFanAbstraction"
              ["size"]
              [TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt]) "nameOFaMapper" ["i", "v"] (Plus (Var "size" False) (Var "v" False))]
          )

    it "Merges types correctly: n->n # n->n => n->n with type vars" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "a", TVar "a", TVar "b", TVar "b"])])
        (GTransformType M.empty [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "b", TVar "c", TVar "c", TVar "d"])] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Var "joinedsize0" False) GaiwanInt] [GaiwanBuf (Var "joinedsize0" False) GaiwanInt])

    it "Merges types correctly: n->n # n->n => n->n" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Var "joinedsize0" False) GaiwanInt] [GaiwanBuf (Var "joinedsize0" False) GaiwanInt])

    it "Merges types correctly: n->n+2 # 2n-> n => 2n -> n+1" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize0" False)) GaiwanInt] [GaiwanBuf (Plus (Var "joinedsize0" False) (Int 1)) GaiwanInt])

    it "Merges types correctly: n->(n+2, n+2) # (2n, 4n) => ERROR" $
      mergeTStr
        ( GTransformType
            M.empty
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
            [ GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt,
              GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt
            ]
        )
        ( GTransformType
            M.empty
            [ GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt,
              GaiwanBuf (Times (Int 4) (Var "n" False)) GaiwanInt
            ]
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
        )
        `shouldSatisfy` isLeft

    it "Merges types correctly: n->(n+2, 2n+4) # (2n, 4n)-> n => 2n -> n+1" $
      mergeTStr
        ( GTransformType
            M.empty
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
            [ GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt,
              GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 4)) GaiwanInt
            ]
        )
        ( GTransformType
            M.empty
            [ GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt,
              GaiwanBuf (Times (Int 4) (Var "n" False)) GaiwanInt
            ]
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
        )
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize0" False)) GaiwanInt] [GaiwanBuf (Plus (Var "joinedsize0" False) (Int 1)) GaiwanInt])

    it "Merges types correctly: n->(n+2, 19, 2n+4) # (2n, 19,4n)-> n => 2n -> n+1" $
      mergeTStr
        ( GTransformType
            M.empty
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
            [ GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt,
              GaiwanBuf (Int 19) GaiwanInt,
              GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 4)) GaiwanInt
            ]
        )
        ( GTransformType
            M.empty
            [ GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt,
              GaiwanBuf (Int 19) GaiwanInt,
              GaiwanBuf (Times (Int 4) (Var "n" False)) GaiwanInt
            ]
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
        )
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize2" False)) GaiwanInt] [GaiwanBuf (Plus (Var "joinedsize2" False) (Int 1)) GaiwanInt])

    it "Merges types correctly: n->(2n+4, n+2) # (4n, 2n)-> n => 2n -> n+1" $
      mergeTStr
        ( GTransformType
            M.empty
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
            [ GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 4)) GaiwanInt,
              GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt
            ]
        )
        ( GTransformType
            M.empty
            [ GaiwanBuf (Times (Int 4) (Var "n" False)) GaiwanInt,
              GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt
            ]
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
        )
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize0" False)) GaiwanInt] [GaiwanBuf (Plus (Var "joinedsize0" False) (Int 1)) GaiwanInt])

    it "Merges types correctly: n->(2n+4, n+2) # (4, 2n)-> n => 0 -> 1" $
      mergeTStr
        ( GTransformType
            M.empty
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
            [ GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 4)) GaiwanInt,
              GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt
            ]
        )
        ( GTransformType
            M.empty
            [ GaiwanBuf (Int 4) GaiwanInt,
              GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt
            ]
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
        )
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Int 0) GaiwanInt] [GaiwanBuf (Int 1) GaiwanInt])

    it "Merges types correctly: n->(2n+4, n+2) # (4, 2n)-> n => 0 -> 1" $
      mergeTStr
        ( GTransformType
            M.empty
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
            [ GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt,
              GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 4)) GaiwanInt
            ]
        )
        ( GTransformType
            M.empty
            [ GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt,
              GaiwanBuf (Int 4) GaiwanInt
            ]
            [ GaiwanBuf (Var "n" False) GaiwanInt
            ]
        )
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Int 0) GaiwanInt] [GaiwanBuf (Int 1) GaiwanInt])

    it "Merges types correctly: n->n+2 # 2n+1->n  => 2n+1->n+1" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 1)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 2) (Var "joinedsize0" False)) (Int 1)) GaiwanInt] [GaiwanBuf (Plus (Var "joinedsize0" False) (Int 1)) GaiwanInt])
    it "Merges types correctly: 2n->n+2 # 3n+1->n => 6n+4->n+1" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Times (Var "n" False) (Int 2)) GaiwanInt] [GaiwanBuf (Plus (Var "n" False) (Int 2)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 6) (Var "joinedsize0" False)) (Int 4)) GaiwanInt] [GaiwanBuf (Plus (Var "joinedsize0" False) (Int 1)) GaiwanInt])
    it "Merges types correctly: 2n->9n+1 # 3n+1->n => 2n->3n" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Times (Var "n" False) (Int 2)) GaiwanInt] [GaiwanBuf (Plus (Times (Int 9) (Var "n" False)) (Int 1)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize0" False)) GaiwanInt] [GaiwanBuf (Times (Int 3) (Var "joinedsize0" False)) GaiwanInt])
    it "Merges types correctly: 2n->9n+4 # 3n+1->n => 2n->3n + 1" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Times (Var "n" False) (Int 2)) GaiwanInt] [GaiwanBuf (Plus (Times (Int 9) (Var "n" False)) (Int 4)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize0" False)) GaiwanInt] [GaiwanBuf (Plus (Times (Int 3) (Var "joinedsize0" False)) (Int 1)) GaiwanInt])
    it "Merges types correctly: n->3n+1 # 9n+4 -> n => 3n+1 -> n " $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt] [GaiwanBuf (Plus (Times (Int 3) (Var "n" False)) (Int 1)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 9) (Var "n" False)) (Int 4)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 3) (Var "joinedsize0" False)) (Int 1)) GaiwanInt] [GaiwanBuf (Var "joinedsize0" False) GaiwanInt])
    it "Merges types correctly: n->11n+2 # n+3->n => n->11n-1" $ -- TODO
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt] [GaiwanBuf (Plus (Times (Int 11) (Var "n" False)) (Int 2)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 1) (Var "n" False)) (Int 3)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Var "joinedsize0" False) GaiwanInt] [GaiwanBuf (Plus (Times (Int 11) (Var "joinedsize0" False)) (Int (-1))) GaiwanInt])
    it "Merges types correctly: n->11n+2 # 7n+3->n => 7n+2->11n+3" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt] [GaiwanBuf (Plus (Times (Int 11) (Var "n" False)) (Int 2)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 7) (Var "n" False)) (Int 3)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 7) (Var "joinedsize0" False)) (Int 2)) GaiwanInt] [GaiwanBuf (Plus (Times (Int 11) (Var "joinedsize0" False)) (Int 3)) GaiwanInt])
    it "Merges types correctly: n->100-n # 200-2n->n => n->n+100" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 100)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 200)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Var "joinedsize0" False) GaiwanInt] [GaiwanBuf (Plus (Var "joinedsize0" False) (Int 100)) GaiwanInt])
    it "Merges types correctly: n->100-n # 200-2n->n => 2n->n+50" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 100)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int $ -2) (Var "n" False)) (Int 200)) GaiwanInt] [GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize0" False)) GaiwanInt] [GaiwanBuf (Plus (Var "joinedsize0" False) (Int 50)) GaiwanInt])
    it "Merges types correctly: n->100-n # 2n->n => 2n->50-n" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Plus (Times (Int $ -1) (Var "n" False)) (Int 100)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 0)) GaiwanInt] [GaiwanBuf (Times (Var "n" False) (Int 1)) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize0" False)) GaiwanInt] [GaiwanBuf (Plus (Times (Int (-1)) (Var "joinedsize0" False)) (Int 50)) GaiwanInt])
    it "Merges types correctly: n->2n+2 # 2n->50-n => n->49-n" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 2)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 0)) GaiwanInt] [GaiwanBuf (Plus (Times (Var "n" False) (Int $ -1)) (Int 50)) GaiwanInt])
        `shouldBe` Right (GTransformType M.empty [GaiwanBuf (Var "joinedsize0" False) GaiwanInt] [GaiwanBuf (Plus (Times (Int (-1)) (Var "joinedsize0" False)) (Int 49)) GaiwanInt])
    it "Merges types correctly: n->2n+1 # 2n->n => FAIL" $
      mergeTStr
        (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Times (Var "n" False) (Int 2)) GaiwanInt])
        (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 1)) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt])
        `shouldSatisfy` isLeft

    -- Handle empty case

    it "types a two mapper in an abstraction" $
      toTypedSmtSimple
        ( Abstraction
            Nothing
            "nameOFanAbstraction"
            [("size", Just (AShape GaiwanInt))]
            [ Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (AShape GaiwanInt))] (Plus (Var "size" False) v),
              Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (AShape GaiwanInt))] (Plus (Var "size" False) v)
            ]
        )
        `shouldBe` Right (TAbstraction (GaiwanArrow [GaiwanInt] (GTransformType M.empty [GaiwanBuf (Var "joinedsize0" False) GaiwanInt] [GaiwanBuf (Var "joinedsize0" False) GaiwanInt])) "nameOFanAbstraction" ["size"] [TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt]) "nameOFaMapper" ["i", "v"] (Plus (Var "size" False) (Var "v" False)), TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt]) "nameOFaMapper" ["i", "v"] (Plus (Var "size" False) (Var "v" False))])

    it "types a two mapper in an abstraction with var" $
      toTypedSmtSimple
        ( Abstraction
            Nothing
            "nameOFanAbstraction"
            [("size", Just $ AShape GaiwanInt)]
            [ Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (AShape GaiwanInt))] (Plus (Var "size" False) v),
              Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (AShape (TVar "Out")))] v,
              Mapper Nothing "nameOFaMapper" [("i", Nothing), ("v", Just (AShape (TVar "Out")))] (Tuple [v, v])
            ]
        )
        `shouldBe` Right (TAbstraction (GaiwanArrow [GaiwanInt] (GTransformType M.empty [GaiwanBuf (Var "joinedsize3" False) GaiwanInt] [GaiwanBuf (Var "joinedsize3" False) (GaiwanTuple [GaiwanInt, GaiwanInt])])) "nameOFanAbstraction" ["size"] [TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) GaiwanInt] [GaiwanBuf (Var "n" False) GaiwanInt]) "nameOFaMapper" ["i", "v"] (Plus (Var "size" False) (Var "v" False)), TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) (TVar 0)] [GaiwanBuf (Var "n" False) (TVar 0)]) "nameOFaMapper" ["i", "v"] (Var "v" False), TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) (TVar 1)] [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar 1, TVar 1])]) "nameOFaMapper" ["i", "v"] (Tuple [Var "v" False, Var "v" False])])

    it "types a two mapper in an abstraction with var" $
      toTypedSmtSimple
        ( Abstraction
            Nothing
            "nameOFanAbstraction"
            [("size", Just (AShape GaiwanInt))]
            [ Shaper
                (Just (ABuf (GaiwanBuf n (GaiwanTuple [TVar "A", TVar "A"]))))
                "nameOFaMapper"
                [ ("i", Nothing),
                  ("v", Just (ABuf (GaiwanBuf (Times (Int 2) n) (TVar "A"))))
                ]
                $ Tuple
                  [ ArrayGet v (Times (Int 2) (Var "i" False)),
                    ArrayGet v (Plus (Int 1) (Times (Int 2) (Var "i" False)))
                  ],
              Shaper
                (Just (ABuf (GaiwanBuf n (GaiwanTuple [TVar "B", TVar "B"]))))
                "nameOFaMapper"
                [("i", Nothing), ("v", Just (ABuf (GaiwanBuf (Plus (Times (Int 2) n) (Int 1)) (TVar "B"))))]
                $ Tuple
                  [ ArrayGet v (Times (Int 2) (Var "i" False)),
                    ArrayGet v (Plus (Int 1) (Times (Int 2) (Var "i" False)))
                  ],
              Mapper
                Nothing
                "nameOFaMapper"
                [ ("i", Nothing),
                  ("v", Just (AShape (GaiwanTuple [GaiwanTuple [GaiwanInt, GaiwanInt], GaiwanTuple [GaiwanInt, GaiwanInt]])))
                ]
                (Plus (Select (Select v 1) 1) (Select (Select v 1) 0))
            ]
        )
        `shouldBe` Right (TAbstraction (GaiwanArrow [GaiwanInt] (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 4) (Var "joinedsize3" False)) (Int 2)) GaiwanInt] [GaiwanBuf (Var "joinedsize3" False) GaiwanInt])) "nameOFanAbstraction" ["size"] [TShaper (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar 0)] [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar 0, TVar 0])]) "nameOFaMapper" ["i", "v"] (Tuple [ArrayGet (Var "v" False) (Times (Int 2) (Var "i" False)), ArrayGet (Var "v" False) (Plus (Int 1) (Times (Int 2) (Var "i" False)))]), TShaper (GTransformType M.empty [GaiwanBuf (Plus (Times (Int 2) (Var "n" False)) (Int 1)) (TVar 1)] [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar 1, TVar 1])]) "nameOFaMapper" ["i", "v"] (Tuple [ArrayGet (Var "v" False) (Times (Int 2) (Var "i" False)), ArrayGet (Var "v" False) (Plus (Int 1) (Times (Int 2) (Var "i" False)))]), TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanTuple [GaiwanInt, GaiwanInt], GaiwanTuple [GaiwanInt, GaiwanInt]])] [GaiwanBuf (Var "n" False) GaiwanInt]) "nameOFaMapper" ["i", "v"] (Plus (Select (Select (Var "v" False) 1) 1) (Select (Select (Var "v" False) 1) 0))])
  describe "Language.Gaiwan simple LetB Return" $ do
    let prog = Prog [] [LetB "k" [IApp "fresh" True [Int 33554432]] [Return ["k", "unbound"]]]
    it "does not parse garbage" $
      checkType prog
        `shouldBe` Right (TypedProg (GTransformType (M.fromList [("unbound", GaiwanBuf (Var "freshlen_2" False) (TVar 32))]) [] [GaiwanBuf (Int 33554432) GaiwanInt, GaiwanBuf (Var "freshlen_2" False) (TVar 32)]) [TLetB (GTransformType (M.fromList [("unbound", GaiwanBuf (Var "freshlen_2" False) (TVar 32))]) [] [GaiwanBuf (Int 33554432) GaiwanInt, GaiwanBuf (Var "freshlen_2" False) (TVar 32)]) "k" [TIApp (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) (TAbstraction (GaiwanArrow [] (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt])) "fresh" [] [TShaper (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) "fresh" ["i"] (Var "i" False)]) []] [TRetrun (GTransformType (M.fromList [("k", GaiwanBuf (Var "freshlen_0" False) (TVar 1)), ("unbound", GaiwanBuf (Var "freshlen_2" False) (TVar 3))]) [] [GaiwanBuf (Var "freshlen_0" False) (TVar 1), GaiwanBuf (Var "freshlen_2" False) (TVar 3)]) ["k", "unbound"]]])

  describe "Language.Gaiwan simple LetB Return" $ do
    let prog = Prog [] [LetB "k" [IApp "fresh" True [Int 33554432]] [Return ["k"]]]
    it "Correcty combines" $
      checkType prog
        `shouldBe` Right (TypedProg (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) [TLetB (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) "k" [TIApp (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) (TAbstraction (GaiwanArrow [] (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt])) "fresh" [] [TShaper (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) "fresh" ["i"] (Var "i" False)]) []] [TRetrun (GTransformType (M.fromList [("k", GaiwanBuf (Var "freshlen_0" False) (TVar 1))]) [] [GaiwanBuf (Var "freshlen_0" False) (TVar 1)]) ["k"]]])

  describe "Language.Gaiwan simple Return" $ do
    let prog = Prog [] [Return ["k"]]
    it "Simple return" $
      checkType prog
        `shouldBe` Right (TypedProg (GTransformType (M.fromList [("k", GaiwanBuf (Var "freshlen_0" False) (TVar 1))]) [] [GaiwanBuf (Var "freshlen_0" False) (TVar 1)]) [TRetrun (GTransformType (M.fromList [("k", GaiwanBuf (Var "freshlen_0" False) (TVar 1))]) [] [GaiwanBuf (Var "freshlen_0" False) (TVar 1)]) ["k"]])

    let prog = Prog [] [Return ["a", "b"]]
    it "Simple return of two" $
      checkType prog
        `shouldBe` Right (TypedProg (GTransformType (M.fromList [("a", GaiwanBuf (Var "freshlen_0" False) (TVar 1)), ("b", GaiwanBuf (Var "freshlen_2" False) (TVar 3))]) [] [GaiwanBuf (Var "freshlen_0" False) (TVar 1), GaiwanBuf (Var "freshlen_2" False) (TVar 3)]) [TRetrun (GTransformType (M.fromList [("a", GaiwanBuf (Var "freshlen_0" False) (TVar 1)), ("b", GaiwanBuf (Var "freshlen_2" False) (TVar 3))]) [] [GaiwanBuf (Var "freshlen_0" False) (TVar 1), GaiwanBuf (Var "freshlen_2" False) (TVar 3)]) ["a", "b"]])

    let def =
          Abstraction
            Nothing
            "nameOfAShaper"
            []
            [ Shaper
                (Just (ABuf (GaiwanBuf n (GaiwanTuple [TVar "a", TVar "a"]))))
                "nameOFaMapper"
                [ ("i", Nothing),
                  ("v", Just (ABuf (GaiwanBuf n (TVar "a")))),
                  ("w", Just (ABuf (GaiwanBuf n (TVar "a"))))
                ]
                ( Tuple
                    [ ArrayGet (Var "v" False) (Var "i" False),
                      ArrayGet (Var "w" False) (Var "i" False)
                    ]
                ),
              Mapper
                Nothing
                "nameOFaMapper"
                [ ("i", Nothing),
                  ("v", Just (AShape $ GaiwanTuple [GaiwanInt, GaiwanInt]))
                ]
                (Plus (Select (Var "v" False) 0) (Select (Var "v" False) 1))
            ]
    let prog = Prog [def] [Return ["a", "b"], IApp "nameOfAShaper" False []]
    it "Simple return of two" $
      checkType prog
        `shouldBe` Right (TypedProg (GTransformType (M.fromList [("a", GaiwanBuf (Var "joinedsize6" False) GaiwanInt), ("b", GaiwanBuf (Var "joinedsize7" False) GaiwanInt)]) [] [GaiwanBuf (Var "joinedsize7" False) GaiwanInt]) [TRetrun (GTransformType (M.fromList [("a", GaiwanBuf (Var "freshlen_2" False) (TVar 3)), ("b", GaiwanBuf (Var "freshlen_4" False) (TVar 5))]) [] [GaiwanBuf (Var "freshlen_2" False) (TVar 3), GaiwanBuf (Var "freshlen_4" False) (TVar 5)]) ["a", "b"], TIApp (GTransformType M.empty [GaiwanBuf (Var "joinedsize1" False) GaiwanInt, GaiwanBuf (Var "joinedsize1" False) GaiwanInt] [GaiwanBuf (Var "joinedsize1" False) GaiwanInt]) (TAbstraction (GaiwanArrow [] (GTransformType M.empty [GaiwanBuf (Var "joinedsize1" False) GaiwanInt, GaiwanBuf (Var "joinedsize1" False) GaiwanInt] [GaiwanBuf (Var "joinedsize1" False) GaiwanInt])) "nameOfAShaper" [] [TShaper (GTransformType M.empty [GaiwanBuf (Var "n" False) (TVar 0), GaiwanBuf (Var "n" False) (TVar 0)] [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar 0, TVar 0])]) "nameOFaMapper" ["i", "v", "w"] (Tuple [ArrayGet (Var "v" False) (Var "i" False), ArrayGet (Var "w" False) (Var "i" False)]), TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanInt, GaiwanInt])] [GaiwanBuf (Var "n" False) GaiwanInt]) "nameOFaMapper" ["i", "v"] (Plus (Select (Var "v" False) 0) (Select (Var "v" False) 1))]) []])

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

    let expectedProg = Prog [Abstraction Nothing "bitonic_select" [("round", Just (AShape GaiwanInt)), ("arrPerBlock", Just (AShape GaiwanInt))] [Shaper (Just (ABuf (GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "C", TVar "C"])))) "split" [("i", Nothing), ("d", Just (ABuf (GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar "C"))))] (Let "blockid" (Div (Var "i" False) (Var "arrPerBlock" False)) (Let "blockstart" (Times (Times (Var "blockid" False) (Var "arrPerBlock" False)) (Int 2)) (Let "blockoffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "pos" (Plus (Var "blockstart" False) (Var "blockoffset" False)) (Tuple [ArrayGet (Var "d" False) (Var "pos" False), ArrayGet (Var "d" False) (Plus (Var "pos" False) (Var "arrPerBlock" False))]))))), Mapper (Just (AShape (GaiwanTuple [GaiwanInt, GaiwanInt]))) "bitonic_select_impl" [("i", Nothing), ("a", Just (AShape (GaiwanTuple [GaiwanInt, GaiwanInt])))] (If (IsGreater (Modulo (Var "i" False) (Pow (Int 2) (Plus (Var "round" False) (Int 1)))) (Pow (Int 2) (Var "round" False))) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Var "a" False) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0])) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0]) (Var "a" False))), Shaper (Just (ABuf (GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar "B")))) "join" [("i", Nothing), ("d", Just (ABuf (GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "B", TVar "B"]))))] (Let "arrowBlock" (Div (Var "i" False) (Times (Int 2) (Var "arrPerBlock" False))) (Let "arrowBlockStart" (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Let "arrowOffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "arrow" (ArrayGet (Var "d" False) (Plus (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Var "arrowOffset" False))) (If (IsGreater (Plus (Times (Var "arrowBlockStart" False) (Int 2)) (Var "arrPerBlock" False)) (Var "i" False)) (Select (Var "arrow" False) 0) (Select (Var "arrow" False) 1))))))], Abstraction Nothing "randomizer" [] [Shaper (Just (ABuf (GaiwanBuf (Var "n" False) GaiwanInt))) "randomizer" [("i", Nothing), ("lol", Just (ABuf (GaiwanBuf (Var "n" False) (TVar "A"))))] (Modulo (Times (Var "i" False) (Int 593)) (Int 1000))]] [IApp "fresh" True [Int 33554432], IApp "randomizer" False [], Loop (Int 25) "round" [Loop (Plus (Var "round" False) (Int 1)) "step" [IApp "bitonic_select" False [Var "round" False, Pow (Int 2) (Minus (Var "round" False) (Var "step" False))]]]]
    it "parses a sort.t program correcty" $ do
      d <- readFile "demo/sort.t"
      parseGaiwan d
        `shouldBe` Right expectedProg
    it "types a sort.t program correcty" $
      checkDefsType expectedProg
        `shouldBe` Right [TAbstraction (GaiwanArrow [GaiwanInt, GaiwanInt] (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt] [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt])) "bitonic_select" ["round", "arrPerBlock"] [TShaper (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar 0)] [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar 0, TVar 0])]) "split" ["i", "d"] (Let "blockid" (Div (Var "i" False) (Var "arrPerBlock" False)) (Let "blockstart" (Times (Times (Var "blockid" False) (Var "arrPerBlock" False)) (Int 2)) (Let "blockoffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "pos" (Plus (Var "blockstart" False) (Var "blockoffset" False)) (Tuple [ArrayGet (Var "d" False) (Var "pos" False), ArrayGet (Var "d" False) (Plus (Var "pos" False) (Var "arrPerBlock" False))]))))), TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanInt, GaiwanInt])] [GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanInt, GaiwanInt])]) "bitonic_select_impl" ["i", "a"] (If (IsGreater (Modulo (Var "i" False) (Pow (Int 2) (Plus (Var "round" False) (Int 1)))) (Pow (Int 2) (Var "round" False))) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Var "a" False) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0])) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0]) (Var "a" False))), TShaper (GTransformType M.empty [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar 1, TVar 1])] [GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar 1)]) "join" ["i", "d"] (Let "arrowBlock" (Div (Var "i" False) (Times (Int 2) (Var "arrPerBlock" False))) (Let "arrowBlockStart" (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Let "arrowOffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "arrow" (ArrayGet (Var "d" False) (Plus (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Var "arrowOffset" False))) (If (IsGreater (Plus (Times (Var "arrowBlockStart" False) (Int 2)) (Var "arrPerBlock" False)) (Var "i" False)) (Select (Var "arrow" False) 0) (Select (Var "arrow" False) 1))))))], TAbstraction (GaiwanArrow [] (GTransformType M.empty [GaiwanBuf (Var "n" False) (TVar 2)] [GaiwanBuf (Var "n" False) GaiwanInt])) "randomizer" [] [TShaper (GTransformType M.empty [GaiwanBuf (Var "n" False) (TVar 2)] [GaiwanBuf (Var "n" False) GaiwanInt]) "randomizer" ["i", "lol"] (Modulo (Times (Var "i" False) (Int 593)) (Int 1000))]]
    it "types a sort.t program correcty" $
      checkType expectedProg
        `shouldBe` Right (TypedProg (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) [TIApp (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) (TAbstraction (GaiwanArrow [] (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt])) "fresh" [] [TShaper (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) "fresh" ["i"] (Var "i" False)]) [], TIApp (GTransformType M.empty [GaiwanBuf (Var "n" False) (TVar 2)] [GaiwanBuf (Var "n" False) GaiwanInt]) (TAbstraction (GaiwanArrow [] (GTransformType M.empty [GaiwanBuf (Var "n" False) (TVar 2)] [GaiwanBuf (Var "n" False) GaiwanInt])) "randomizer" [] [TShaper (GTransformType M.empty [GaiwanBuf (Var "n" False) (TVar 2)] [GaiwanBuf (Var "n" False) GaiwanInt]) "randomizer" ["i", "lol"] (Modulo (Times (Var "i" False) (Int 593)) (Int 1000))]) [], TLoop (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt] [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt]) (Int 25) "round" [TLoop (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt] [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt]) (Plus (Var "round" False) (Int 1)) "step" [TIApp (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt] [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt]) (TAbstraction (GaiwanArrow [GaiwanInt, GaiwanInt] (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt] [GaiwanBuf (Times (Int 2) (Var "joinedsize4" False)) GaiwanInt])) "bitonic_select" ["round", "arrPerBlock"] [TShaper (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar 0)] [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar 0, TVar 0])]) "split" ["i", "d"] (Let "blockid" (Div (Var "i" False) (Var "arrPerBlock" False)) (Let "blockstart" (Times (Times (Var "blockid" False) (Var "arrPerBlock" False)) (Int 2)) (Let "blockoffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "pos" (Plus (Var "blockstart" False) (Var "blockoffset" False)) (Tuple [ArrayGet (Var "d" False) (Var "pos" False), ArrayGet (Var "d" False) (Plus (Var "pos" False) (Var "arrPerBlock" False))]))))), TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanInt, GaiwanInt])] [GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanInt, GaiwanInt])]) "bitonic_select_impl" ["i", "a"] (If (IsGreater (Modulo (Var "i" False) (Pow (Int 2) (Plus (Var "round" False) (Int 1)))) (Pow (Int 2) (Var "round" False))) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Var "a" False) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0])) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0]) (Var "a" False))), TShaper (GTransformType M.empty [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar 1, TVar 1])] [GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar 1)]) "join" ["i", "d"] (Let "arrowBlock" (Div (Var "i" False) (Times (Int 2) (Var "arrPerBlock" False))) (Let "arrowBlockStart" (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Let "arrowOffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "arrow" (ArrayGet (Var "d" False) (Plus (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Var "arrowOffset" False))) (If (IsGreater (Plus (Times (Var "arrowBlockStart" False) (Int 2)) (Var "arrPerBlock" False)) (Var "i" False)) (Select (Var "arrow" False) 0) (Select (Var "arrow" False) 1))))))]) [Var "round" False, Pow (Int 2) (Minus (Var "round" False) (Var "step" False))]]]])

--     let expectedTypedShort =
--           ( TypedProg
--               [ TIApp
--                   ( GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]
--                   )
--                   (TAbstraction (GaiwanArrow [] (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt])) "fresh" [] [TShaper (GTransformType M.empty [] [GaiwanBuf (Int 33554432) GaiwanInt]) "fresh" ["i"] (Var "i" False)])
--                   [],
--                 TIApp (GTransformType M.empty [] [GaiwanBuf (Var "n" False) GaiwanInt]) (TAbstraction (GaiwanArrow [] (GTransformType M.empty [] [GaiwanBuf (Var "n" False) GaiwanInt])) "randomizer" [] [TShaper (GTransformType M.empty [] [GaiwanBuf (Var "n" False) GaiwanInt]) "randomizer" ["i"] (Modulo (Times (Var "i" False) (Int 593)) (Int 1000))]) [],
--                 TLoop (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt] [GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt]) (Int 2) "round" [TLoop (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt] [GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt]) (Plus (Var "round" False) (Int 1)) "step" [TIApp (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt] [GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt]) (TAbstraction (GaiwanArrow [GaiwanInt, GaiwanInt] (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt] [GaiwanBuf (Times (Int 2) (Var "n" False)) GaiwanInt])) "bitonic_select" ["round", "arrPerBlock"] [TShaper (GTransformType M.empty [GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar "C")] [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "C", TVar "C"])]) "split" ["i", "d"] (Let "blockid" (Div (Var "i" False) (Var "arrPerBlock" False)) (Let "blockstart" (Times (Times (Var "blockid" False) (Var "arrPerBlock" False)) (Int 2)) (Let "blockoffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "pos" (Plus (Var "blockstart" False) (Var "blockoffset" False)) (Tuple [ArrayGet (Var "d" False) (Var "pos" False), ArrayGet (Var "d" False) (Plus (Var "pos" False) (Var "arrPerBlock" False))]))))), TMapper (GTransformType M.empty [GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanInt, GaiwanInt])] [GaiwanBuf (Var "n" False) (GaiwanTuple [GaiwanInt, GaiwanInt])]) "bitonic_select_impl" ["i", "a"] (If (IsGreater (Modulo (Var "i" False) (Pow (Int 2) (Plus (Var "round" False) (Int 1)))) (Pow (Int 2) (Var "round" False))) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Var "a" False) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0])) (If (IsGreater (Select (Var "a" False) 0) (Select (Var "a" False) 1)) (Tuple [Select (Var "a" False) 1, Select (Var "a" False) 0]) (Var "a" False))), TShaper (GTransformType M.empty [GaiwanBuf (Var "n" False) (GaiwanTuple [TVar "B", TVar "B"])] [GaiwanBuf (Times (Int 2) (Var "n" False)) (TVar "B")]) "join" ["i", "d"] (Let "arrowBlock" (Div (Var "i" False) (Times (Int 2) (Var "arrPerBlock" False))) (Let "arrowBlockStart" (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Let "arrowOffset" (Modulo (Var "i" False) (Var "arrPerBlock" False)) (Let "arrow" (ArrayGet (Var "d" False) (Plus (Times (Var "arrowBlock" False) (Var "arrPerBlock" False)) (Var "arrowOffset" False))) (If (IsGreater (Plus (Times (Var "arrowBlockStart" False) (Int 2)) (Var "arrPerBlock" False)) (Var "i" False)) (Select (Var "arrow" False) 0) (Select (Var "arrow" False) 1))))))]) [Var "round" False, Pow (Int 2) (Minus (Var "round" False) (Var "step" False))]]]
--               ]
--           )

--     it "plans a sort.t program correcty" $ do
--       makePlan expectedTypedShort `shouldBe` []
--       let plan = makePlan expectedTypedShort
--       mapM_ print plan

-- todo: add test for all demos to see if they are `Right _`
