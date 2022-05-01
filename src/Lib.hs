module Lib
  ( Lib.run,
    runCompiled,
    Lib.render,
    Lib.compile,
    checkParse,
    convert,
  )
where

import Code
-- import CodeGen.OpenCL
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
import Language.GaiwanTypes (checkType, backPropagate)
import CodeGen.Pipelining (prepare)
import OpenCL (convertPlan, mkOpenRunner, run)
import Foreign
import Foreign.Storable
import Foreign.C (CInt)

someFunc = Lib.compile

eitherParseGaiwan a b s = either a b $ parseGaiwan s

run :: String -> IO ()
run =
  eitherParseGaiwan
    (\err -> print $ "Parsing failed: " ++ err)
    (convert >=> print)

checkParse :: String -> Either String (Program String)
checkParse =
  eitherParseGaiwan
    (\err -> Left $ "Parsing failed: " ++ err)
    Right

-- Output a bytestring
compile :: String -> Either String BS.ByteString
compile = undefined

--  eitherParseGaiwan
--    (\err -> Left $ "Parsing failed: " ++ err)
--    (Right . uncurry Code.serialize . Code.compile . mkCode) -- todo make either

mkCode (Prog defines main) =
  undefined

-- execCode $ do
-- mapM_ registerDef defines
--- mkOpenCLKernelCode main

convert :: Program String -> IO [[Integer]] -- TODO figure aout what a should be
convert program =  do
    let tp = checkType program >>= (\p -> evalStateT (backPropagate p) 0)
    case tp of
      (Left s) -> do
          print $ "Could not run: " ++ show s
          return []
      (Right typedProgramFull) -> do
          let (code, actions) = prepare typedProgramFull
          plan <- convertPlan actions
          case plan of
            (Left s) -> do
              print $ "Could not convert plan in envionment: " ++ show s
              return []
            (Right (ocactions, defines)) -> do
               runner <- mkOpenRunner convertor code defines
               OpenCL.run runner ocactions

convertor :: Int -> Ptr CInt -> IO [Integer]
convertor size ptr = map toInteger <$> mapM (peekElemOff ptr) [0 .. (min size 100) - 1]


-- runOpenCL $ mkCode program

render :: String -> Either String BS.ByteString
render =
  eitherParseGaiwan
    (\err -> Left $ "Parsing failed: " ++ err)
    (Right . Render.renderJSON)

runCompiled = undefined
