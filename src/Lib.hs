module Lib
  ( run,
    runCompiled,
    Lib.compile,
    checkParse,
    convert,
  )
where

import Code
import CodeGen.OpenCL
-- import OpenCL

import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe
import Language.Gaiwan
import Language.GaiwanDefs
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

mkCode (Prog defines main) =
  execCode $ do
    mapM_ registerDef defines
    convertMain main

convert :: Program -> IO [[Integer]]
convert program =
  runCodeToList $ mkCode program

funPrefix True = "int_"
funPrefix False = "user_"

-- Convert main
convertMain :: Exp -> SCode ()
convertMain main =
  void $ mkOpenCLKernelCode main

-- todo make less ugly
