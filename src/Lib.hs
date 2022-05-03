module Lib
  ( Lib.run,
    runCompiled,
    Lib.render,
    Lib.compile,
    checkParse,
    convert,
    convertTyped,
    convertPrepared,
  )
where

import Code
-- import CodeGen.OpenCL
-- import OpenCL

import CodeGen.Pipelining (prepare)
import Control.Monad
import Control.Monad.State.Lazy
import qualified Data.ByteString.Lazy as BS
import Data.List
import Data.Maybe
import Debug.Trace
import Foreign
import Foreign.C (CInt)
import Foreign.Storable
import Language.Gaiwan
import Language.GaiwanDefs
import Language.GaiwanTypes (TypedProgram, backPropagate, checkType)
import OpenCL (convertPlan, mkOpenRunner, run)
import Render
import System.Exit

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
convert program = do
  traceM "pre check type"
  let tp = checkType program >>= (\p -> evalStateT (backPropagate p) 0)
  case tp of
    (Left s) -> do
      print $ "Could not run: " ++ show s
      return []
    (Right typedProgramFull) -> convertTyped typedProgramFull

convertTyped :: TypedProgram -> IO [[Integer]]
convertTyped typedProgramFull = do
  traceM "pre plan made"
  let (code, actions) = prepare typedProgramFull
  convertPrepared code actions

convertPrepared :: String -> [GPUAction] -> IO [[Integer]]
convertPrepared code actions = do
  plan <- convertPlan actions
  traceM "plan made"
  case plan of
    (Left s) -> do
      print $ "Could not convert plan in envionment: " ++ show s
      return []
    (Right (ocactions, defines)) -> do
      runner <- mkOpenRunner convertor code defines
      traceM "runner made"
      OpenCL.run runner ocactions

convertor :: Int -> Ptr CInt -> IO [Integer]
convertor size ptr = map toInteger <$> mapM (peekElemOff ptr) ([0 .. (min size 100) - 1] ++ ([size - 2, size - 1]))

-- runOpenCL $ mkCode program

render :: String -> Either String BS.ByteString
render =
  eitherParseGaiwan
    (\err -> Left $ "Parsing failed: " ++ err)
    (Right . Render.renderJSON)

runCompiled = undefined
