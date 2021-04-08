{-# LANGUAGE BangPatterns #-}

module Main where

import Criterion.Main
import qualified Data.ByteString.Lazy as BS
import Data.Either
import Lib
import System.Environment.Blank

main :: IO ()
main = do
  code <- readFile "demo/sort.t"
  -- Bang means strict evaluation
  -- We need to ensure that this is not counted in the benchmark
  let !c = BS.toStrict $ fromRight (error "Could not compile") $ compile code
  let !d = BS.fromStrict c
  defaultMain [bgroup "sort" [bench "test" $ nfIO $ runCompiled d >>= (\x -> return (head . head <$> x))]]
