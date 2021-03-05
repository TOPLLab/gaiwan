{-# LANGUAGE QuasiQuotes #-}

module RenderSpec (spec) where

import qualified Data.ByteString.Lazy as BS
import Language.Gaiwan
import Language.GaiwanDefs
import Render
import RenderDefs
import Test.Hspec
import Test.QuickCheck
import Text.RawString.QQ

shouldBeJSON :: Pic -> String -> Expectation
shouldBeJSON a b = show (asJSON a) `shouldBe` show b

spec =
  describe "Render" $ do
    it "makes a good model" $ do
      render p
        `shouldBeJSON` [r|[{"buffers":[{"contents":[{"value":0,"sources":[]},{"value":1,"sources":[]}],"id":1},{"contents":[{"value":0,"sources":[]},{"value":1,"sources":[]}],"id":2},{"contents":[{"value":0,"sources":[]},{"value":1,"sources":[]}],"id":3},{"contents":[{"value":0,"sources":[]},{"value":1,"sources":[]}],"id":4}],"comment":"KernelName \"kernel0\""},{"buffers":[{"contents":[{"value":0,"sources":[[1,0]]},{"value":1,"sources":[[1,1]]}],"id":5},{"contents":[{"value":0,"sources":[[2,0]]},{"value":1,"sources":[[2,1]]}],"id":6},{"contents":[{"value":0,"sources":[[3,0]]},{"value":1,"sources":[[3,1]]}],"id":7},{"contents":[{"value":0,"sources":[[4,0]]},{"value":1,"sources":[[4,1]]}],"id":8}],"comment":"KernelName \"kernel1\""},{"buffers":[{"contents":[{"value":1,"sources":[]},{"value":1,"sources":[]}],"id":9},{"contents":[{"value":2,"sources":[]},{"value":2,"sources":[]}],"id":10},{"contents":[{"value":3,"sources":[]},{"value":3,"sources":[]}],"id":11},{"contents":[{"value":4,"sources":[]},{"value":4,"sources":[]}],"id":12}],"comment":"KernelName \"kernel2\""}]|]

    it "makes a good model 2" $ do
      render program
        `shouldBeJSON` [r|[{"buffers":[{"contents":[{"value":0,"sources":[]},{"value":1,"sources":[]},{"value":2,"sources":[]}],"id":1}],"comment":"KernelName \"kernel0\""},{"buffers":[{"contents":[{"value":0,"sources":[[1,0]]},{"value":1,"sources":[[1,1]]},{"value":2,"sources":[[1,2]]}],"id":2}],"comment":"KernelName \"kernel1\""},{"buffers":[{"contents":[{"value":0,"sources":[[2,0]]},{"value":1,"sources":[[2,1]]},{"value":2,"sources":[[2,2]]}],"id":3},{"contents":[{"value":0,"sources":[[2,0]]},{"value":1,"sources":[[2,1]]},{"value":2,"sources":[[2,2]]}],"id":4}],"comment":"KernelName \"kernel2\""},{"buffers":[{"contents":[{"value":0,"sources":[[3,0]]},{"value":2,"sources":[[3,1]]},{"value":4,"sources":[[3,2]]}],"id":5},{"contents":[{"value":2,"sources":[[4,0]]},{"value":3,"sources":[[4,1]]},{"value":4,"sources":[[4,2]]}],"id":6}],"comment":"KernelName \"kernel3\""},{"buffers":[{"contents":[{"value":0,"sources":[[5,0]]},{"value":2,"sources":[[5,1]]},{"value":4,"sources":[[5,2]]}],"id":7},{"contents":[{"value":2,"sources":[[6,0]]},{"value":3,"sources":[[6,1]]},{"value":4,"sources":[[6,2]]}],"id":8}],"comment":"KernelName \"kernel4\""},{"buffers":[{"contents":[{"value":2,"sources":[[7,1]]},{"value":4,"sources":[[7,2]]},{"value":0,"sources":[[7,0]]}],"id":9},{"contents":[{"value":3,"sources":[[8,1]]},{"value":4,"sources":[[8,2]]},{"value":2,"sources":[[8,0]]}],"id":10}],"comment":"KernelName \"kernel5\""},{"buffers":[{"contents":[{"value":4,"sources":[[9,0]]},{"value":8,"sources":[[9,1]]},{"value":0,"sources":[[9,2]]}],"id":11},{"contents":[{"value":5,"sources":[[10,0]]},{"value":6,"sources":[[10,1]]},{"value":4,"sources":[[10,2]]}],"id":12}],"comment":"KernelName \"kernel3\""},{"buffers":[{"contents":[{"value":4,"sources":[[11,0]]},{"value":8,"sources":[[11,1]]},{"value":0,"sources":[[11,2]]}],"id":13},{"contents":[{"value":5,"sources":[[12,0]]},{"value":6,"sources":[[12,1]]},{"value":4,"sources":[[12,2]]}],"id":14}],"comment":"KernelName \"kernel4\""},{"buffers":[{"contents":[{"value":5,"sources":[[14,0]]},{"value":6,"sources":[[14,1]]},{"value":4,"sources":[[14,2]]}],"id":15},{"contents":[{"value":4,"sources":[[13,0]]},{"value":8,"sources":[[13,1]]},{"value":0,"sources":[[13,2]]}],"id":16}],"comment":"KernelName \"kernel6\""},{"buffers":[{"contents":[{"value":5,"sources":[[15,0]]},{"value":6,"sources":[[15,1]]},{"value":4,"sources":[[15,2]]}],"id":17},{"contents":[{"value":4,"sources":[[16,0]]},{"value":8,"sources":[[16,1]]},{"value":0,"sources":[[16,2]]}],"id":18}],"comment":"KernelName \"kernel4\""}]|]

    it "makes a good model with split and join" $ do
      render programSplitAndJoin `shouldBeJSON` programSplitAndJoinOut

p =
  Prog
    [Mapper "do" ["i", "a", "b", "c", "d"] (Int <$> [1, 2, 3, 4])]
    ( PipedExp
        [ App "generateSeq" True [Int 4, Int 2],
          App "do" False []
        ]
    )

programDefines =
  -- TODO consolidate
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
        [ App "generateSeq" True [Int 1, Int 3],
          App "doubler" False [],
          App "haha" False [Int 2],
          App "shift" False [],
          App "haha" False [Int 2],
          App "swap" False [],
          App "id" False []
        ]
    )

programSplitAndJoin =
  Prog
    [ Mapper "id" ["i", "a", "b"] [Var "a" False, Var "b" False],
      Mapper "idt" ["i", "a", "b"] [Times (Int 10) (Var "a" False), Times (Int 100) (Var "b" False)],
      Mapper "id1" ["i", "a"] [Var "a" False]
    ]
    ( PipedExp
        [ App "generateSeq" True [Int 1, Int 8],
          App "split" True [Int 2, Int 1],
          App "idt" False [],
          App "join" True [Int 2, Int 2]
        ]
    )

programSplitAndJoinOut = [r|[{"buffers":[{"contents":[{"value":0,"sources":[]},{"value":1,"sources":[]},{"value":2,"sources":[]},{"value":3,"sources":[]},{"value":4,"sources":[]},{"value":5,"sources":[]},{"value":6,"sources":[]},{"value":7,"sources":[]}],"id":1}],"comment":"KernelName \"kernel0\""},{"buffers":[{"contents":[{"value":0,"sources":[[1,0]]},{"value":1,"sources":[[1,1]]},{"value":2,"sources":[[1,2]]},{"value":3,"sources":[[1,3]]},{"value":4,"sources":[[1,4]]},{"value":5,"sources":[[1,5]]},{"value":6,"sources":[[1,6]]},{"value":7,"sources":[[1,7]]}],"id":2}],"comment":"KernelName \"kernel1\""},{"buffers":[{"contents":[{"value":0,"sources":[[2,0]]},{"value":2,"sources":[[2,2]]},{"value":4,"sources":[[2,4]]},{"value":6,"sources":[[2,6]]}],"id":3},{"contents":[{"value":1,"sources":[[2,1]]},{"value":3,"sources":[[2,3]]},{"value":5,"sources":[[2,5]]},{"value":7,"sources":[[2,7]]}],"id":4}],"comment":"KernelName \"kernel2\""},{"buffers":[{"contents":[{"value":0,"sources":[[3,0]]},{"value":20,"sources":[[3,1]]},{"value":40,"sources":[[3,2]]},{"value":60,"sources":[[3,3]]}],"id":5},{"contents":[{"value":100,"sources":[[4,0]]},{"value":300,"sources":[[4,1]]},{"value":500,"sources":[[4,2]]},{"value":700,"sources":[[4,3]]}],"id":6}],"comment":"KernelName \"kernel3\""},{"buffers":[{"contents":[{"value":0,"sources":[[5,0]]},{"value":20,"sources":[[5,1]]},{"value":40,"sources":[[5,2]]},{"value":60,"sources":[[5,3]]}],"id":7},{"contents":[{"value":100,"sources":[[6,0]]},{"value":300,"sources":[[6,1]]},{"value":500,"sources":[[6,2]]},{"value":700,"sources":[[6,3]]}],"id":8}],"comment":"KernelName \"kernel4\""},{"buffers":[{"contents":[{"value":0,"sources":[[7,0]]},{"value":20,"sources":[[7,1]]},{"value":100,"sources":[[8,0]]},{"value":300,"sources":[[8,1]]},{"value":40,"sources":[[7,2]]},{"value":60,"sources":[[7,3]]},{"value":500,"sources":[[8,2]]},{"value":700,"sources":[[8,3]]}],"id":9}],"comment":"KernelName \"kernel5\""}]|]
