module UnificationSpec (spec) where

import Language.Gaiwan
import Language.GaiwanDefs
import Test.Hspec
import Test.QuickCheck

spec = do
  describe "Language.GaiwanDefs (type)" $ do
    it "replaces array acesses correcty" $
      do
        substArrayGet
          "d"
          (\index -> simpleSubst ("i", True) index (Plus (Int 1) (Var "i" True)))
          (ArrayGet (Var "d" False) (Int 6) :: Exp)
        `shouldBe` Int 7

    it "replaces array acesses correcty" $
      do
        substArrayGet
          "d"
          (\index -> simpleSubst ("i", True) index (Plus (Int 1) (Var "i" True)))
          (Plus (ArrayGet (Var "d" False) (Int 6)) (ArrayGet (Var "d" False) (Int 5)) :: Exp)
        `shouldBe` Int 13

    it "replaces array acesses correcty" $
      do
        substArrayGet
          "d"
          (\index -> simpleSubst ("i", True) index (Plus (Int 1) (Var "i" True)))
          (Plus (ArrayGet (Var "d" False) (Int 6)) (ArrayGet (Var "e" False) (Int 5)) :: Exp)
        `shouldBe` Plus (Int 7) (ArrayGet (Var "e" False) (Int 5))

    let problematicMin = (Tuple [ArrayGet (Var "d" False) (Var "pos" False), ArrayGet (Var "d" False) (Plus (Var "pos" False) (Var "arrPerBlock" False))] :: Exp)

    it "replaces array acesses correcty" $ do
      substArrayGet
        "d"
        (\index -> simpleSubst ("i", True) index (Plus (Int 1) (Var "i" True)))
        problematicMin
        `shouldBe` Tuple [Plus (Int 1) (Var "pos" False), Plus (Int 1) (Plus (Var "pos" False) (Var "arrPerBlock" False))]

    it "lifts commom expression" $ do
      let r = (Times (Var "r" False) (Int 3)) :: GExp Void
      simplifyExp (Plus (Plus (Plus (Plus (Plus r r) (Plus r r)) (Plus (Plus r r) (Plus r r))) (Plus (Plus (Plus r r) (Plus r r)) (Plus (Plus r r) (Plus r r)))) (Plus (Plus (Plus (Plus r r) (Plus r r)) (Plus (Plus r r) (Plus r r))) (Plus (Plus (Plus r r) (Plus r r)) (Plus (Plus r r) (Plus r r)))))
        `shouldBe` Tuple [Plus (Int 1) (Var "pos" False), Plus (Int 1) (Plus (Var "pos" False) (Var "arrPerBlock" False))]
