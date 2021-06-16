module Lib
  ( run,
    runCompiled,
    Lib.render,
    Lib.compile,
    checkParse,
    convert,
  )
where

import Code
-- import OpenCL

import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe
import Language.Gaiwan
import Language.GaiwanDefs
import Render
import System.Exit

someFunc = Lib.compile

eitherParseGaiwan a b s = either a b $ parseGaiwan s

run :: String -> IO ()
run =
  eitherParseGaiwan
    (\err -> print $ "Parsing failed: " ++ err)
    (convert >=> print)

checkParse :: String -> Either String Program
checkParse =
  eitherParseGaiwan
    (\err -> Left $ "Parsing failed: " ++ err)
    Right

-- Output a bytestring
compile :: String -> Either String BS.ByteString
compile =
  eitherParseGaiwan
    (\err -> Left $ "Parsing failed: " ++ err)
    (Right . uncurry Code.serialize . Code.compile . mkCode) -- todo make either

mkCode (Prog defines main) = undefined 

convert :: Program -> IO [[Integer]]
convert program =
  undefined

render :: String -> Either String BS.ByteString
render =
  eitherParseGaiwan
    (\err -> Left $ "Parsing failed: " ++ err)
    (Right . Render.renderJSON)

runCompiled = undefined
