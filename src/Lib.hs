module Lib
    ( someFunc
    ) where

import Language.Gaiwan
import System.Exit

someFunc :: IO ()
someFunc = getContents >>= printG . parseGaiwan
    where
        printG (Left m) = do { putStr "Parsing failed!\n"; putStr m; putStr "\n";exitFailure }
        printG (Right m) = print m


