module TestPrograms where

import Language.GaiwanDefs

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
