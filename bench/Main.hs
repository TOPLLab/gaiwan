{-# LANGUAGE BangPatterns #-}

module Main where

import OpenCL
import Criterion.Main
import qualified Data.ByteString.Lazy as BS
import Data.Either
import Foreign
import Foreign.Storable
import Lib
import System.Environment.Blank

main :: IO ()
main = do
  code <- readFile "demo/sort-input.t"
  let Right p = checkParse code


  -- Bang means strict evaluation
  -- We need to ensure that this is not counted in the benchmark
  let !pp = p
  defaultMain
    [ bgroup
        "sort"
        [ bench "test" $
            nfIO $
              convert pp
        ]
    ]

