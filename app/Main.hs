module Main where

import Lib

main :: IO ()
main = getContents >>= someFunc
