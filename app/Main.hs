module Main where

import qualified Data.ByteString.Lazy as BS
import Lib
import System.Environment.Blank

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["compile"] ->
      getContents >>= either print BS.putStr . compile
    ["compile", file] ->
      readFile file >>= either print BS.putStr . compile
    ["compile", file, fileout] ->
      readFile file >>= either print (BS.writeFile fileout) . compile
    ["run", file] ->
      undefined
    -- BS.readFile file >>= runCompiled >>= print
    ["eval", file] ->
      readFile file >>= run
    ["eval"] ->
      getContents >>= run
    ["render", file] ->
      readFile file >>= either print BS.putStr . render
    ["checkParse", file] ->
      readFile file >>= print . checkParse
    ["checkParse"] ->
      getContents >>= print . checkParse
    _ -> error $ "Unknown argument:" ++ show args
