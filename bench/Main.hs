{-# LANGUAGE BangPatterns #-}

module Main where

import CodeGen.OpenCL
import Criterion.Main
import qualified Data.ByteString.Lazy as BS
import Data.Either
import Foreign
import Foreign.Storable
import Lib
import System.Environment.Blank

main :: IO ()
main = do
  code <- readFile "demo/sort.t"
  -- Bang means strict evaluation
  -- We need to ensure that this is not counted in the benchmark
  let !c = BS.toStrict $ fromRight (error "Could not compile") $ compile code
  let !d = BS.fromStrict c
  defaultMain
    [ bgroup
        "sort"
        [ bench "test" $
            nfIO $
              runOnlyFirst d
        ]
    ]

-- | An openCL Runner that only reads the first 10 elements of each buffer
runOnlyFirst = runOpenCLCompiledWithConv conv
  where
    conv size ptr = map toInteger <$> mapM (peekElemOff ptr) [0 .. min size 10 -1]
