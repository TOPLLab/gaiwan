module Lib
  ( someFunc,
  )
where

import Code
import Control.Monad.State.Lazy
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Language.Gaiwan
import OpenCL
import System.Exit

someFunc :: IO ()
someFunc = getContents >>= printG . parseGaiwan
  where
    printG (Left m) = do
      putStr "Parsing failed!\n"
      putStr m
      putStr "\n"
      exitFailure
    printG (Right m) = convert m

convert :: Program -> IO ()
convert (Prog defines main) =
  runCode $
    execCode $ do
      let km = map kindDefs defines
      mapM convertDef defines
      convertMain (map kindDefs defines) main

type KindsMap = [(String, [Exp] -> PipelineStep)]

kindDefs :: Stmt -> (String, [Exp] -> PipelineStep)
kindDefs (Shuffler name _ _) = (name, PShuffle name)
kindDefs (Mapper name _ _) = (name, PMap name)

convertDef :: Stmt -> State Code ()
convertDef (Shuffler name args body) = do
  kBody <- mkKernelCodeB [] body
  addDeviceCode $
    "int user_"
      ++ name
      ++ "("
      ++ intercalate "," (map ("int arg_" ++) args)
      ++ "){"
      ++ "return "
      ++ kBody
      ++ ";};\n\n"
convertDef (Mapper name args body) = do
  kBody <- mkKernelCodeB [] body
  addDeviceCode $
    "int user_"
      ++ name
      ++ "("
      ++ intercalate "," (map ("int arg_" ++) args)
      ++ "){"
      ++ "return "
      ++ kBody
      ++ ";};\n\n"

fun_prefix True = "int_"
fun_prefix False = "user_"

mkKernelBinOp km a b op = do
  ka <- mkKernelCodeB km a
  kb <- mkKernelCodeB km b
  return $ ka ++ op ++ kb

mkKernelCode :: KindsMap -> Exp -> State Code String
mkKernelCode _ (Let _ _ _) = error "Let not yet supported!"
mkKernelCode km (Plus a b) = mkKernelBinOp km a b "+"
mkKernelCode km (Minus a b) = mkKernelBinOp km a b "-"
mkKernelCode km (Times a b) = mkKernelBinOp km a b "*"
mkKernelCode km (Div a b) = mkKernelBinOp km a b "/"
mkKernelCode km (Modulo a b) = mkKernelBinOp km a b "%"
mkKernelCode km (App f builtin args) = do
  cArgs <- mapM (mkKernelCodeB km) args
  return $ fun_prefix builtin ++ f ++ "(" ++ intercalate "," cArgs ++ ")"
mkKernelCode _ (Int num) = return $ show num
mkKernelCode _ (Var name True) = return $ "int_" ++ name
mkKernelCode _ (Var name False) = return $ "arg_" ++ name
mkKernelCode km (Negate x) = mkKernelCodeB km x >>= (return . ("-" ++))
mkKernelCode km (ArrayGet x idx) = do
  kx <- mkKernelCodeB km x
  kidx <- mkKernelCodeB km idx
  return $ kx ++ "[" ++ kidx ++ "]"
mkKernelCode km (PipedExp expressions) = do
  let pipe = convertPipe km expressions
  mapM addHostCode $ map AllocBuffer (analyseArrays pipe)
  mapM (convertPls km) pipe
  mapM
    addHostCode
    ( map
        ReadBuffer
        $ outBuffer $
          last pipe
    )
  return ""

outBuffer (_, o, _) = o

analyseArrays :: [([GPUBuffer], [GPUBuffer], Exp)] -> [GPUBuffer]
analyseArrays p = Set.toList $ foldl addI Set.empty p
  where
    addI s (i, o, _) = foldr Set.insert s (i ++ o)

--analyseArrays l = zip  (foldr ff [] l) (reverse $ foldl fr [] l)
--    where ff :: ([GPUBuffer],[GPUBuffer], Exp) -> [[GPUBuffer]] -> [[GPUBuffer]]
--          ff (i, o, _) prev@(p:_) = (o++p) : prev
--          ff (i, o, _) []= [o]
--          fr prev@(p:_) (i, o, _) = (p++i) : prev
--          fr [] (i, o, _) = [i,[]]

-- convert pipelinestep
convertPls kv (argIn, argOut@[GPUBuffer outName s], body) = do
  fName <- getAName "kernelfun"
  kBody <- mkKernelCodeB kv body
  addDeviceCode $
    "void kernel "
      ++ fName
      ++ "("
      ++ (intercalate ", " $ map gpuBufferDecl (argIn ++ argOut))
      ++ "){ int int_index = get_global_id(0);"
      ++ "int_" ++ outName
      ++ "[int_index] ="
      ++ kBody
      ++ ";};"
  addHostCode $ MakeKernel fName (argIn ++ argOut) (Range s 0 0)


-- Add brackets
mkKernelCodeB :: KindsMap -> Exp -> State Code String
mkKernelCodeB km x = do
  v <- mkKernelCode km x
  return $ "(" ++ v ++ ")"

-- Convert main
convertMain :: KindsMap -> Exp -> State Code ()
convertMain defs main =
  mkKernelCode defs main >> return ()

-- Pipeline stuff
data Pipeline = Pipeline
  { initP :: Exp,
    stepsP :: [PipelineStep]
  }
  deriving (Show)

data Data = Data
  { shuffle :: Exp,
    requiredArrays :: Int, -- for uniq names
    lastMap :: Bool, -- if the previous step was a map
    exps :: [([GPUBuffer], [GPUBuffer], Exp)],
    curSize :: Int
  }
  deriving (Show)

data PipelineStep = PShuffle String [Exp] | PMap String [Exp]
  deriving (Show)

gpuBufferDecl (GPUBuffer name size) =
  "global int int_" ++ name ++ "[" ++ show size ++ "]"

-- Convert a pipe into kernel specifications
-- todo make datatype with in,out,exp and kernelcode
convertPipe :: KindsMap -> [Exp] -> [([GPUBuffer], [GPUBuffer], Exp)]
convertPipe km (h : r) =
  reverse $
    exps $
      foldl
        convP
        (convH h)
        (map getPipeKind r)
  where
    -- convert first item of the pipe
    convH :: Exp -> Data
    convH (App "generateSeq" True [(Int n)]) =
      ( Data
          { shuffle = Var "index" True,
            requiredArrays = 0,
            lastMap = True,
            exps = [([], [GPUBuffer "array0" n], Var "index" True)],
            curSize = n
          }
      )
    -- converts subsequent steps
    convP x (PShuffle n a) =
      x
        { shuffle = App n False ((Int (curSize x)) : (shuffle x : a)),
          lastMap = False
        }
    convP x@Data {lastMap = True, exps = (iExp, oExp, cExp) : rExp} (PMap n args) =
      x
        { exps = (iExp, oExp, App n False (args ++ [cExp])) : rExp,
          lastMap = True
        }
    convP x@Data {lastMap = False, requiredArrays = ra, shuffle = shuf, exps = (iExp, oExp, cExp) : rExp, curSize = size} (PMap n args) =
      x
        { exps =
            ( oExp,
              [GPUBuffer (arrayName $ ra + 1) size],
              App n False $ makeArrayAccess ra shuf : args
            ) :
            exps x,
          shuffle = Var "index" True,
          lastMap = True,
          requiredArrays = (ra + 1)
        }
    makeArrayAccess :: Int -> Exp -> Exp
    makeArrayAccess name = ArrayGet (Var ("array" ++ show name) True)
    arrayName num = "array" ++ show num
    getPipeKind :: Exp -> PipelineStep
    getPipeKind (App name False args) =
      fromMaybe (PMap "err") (lookup name km) args

-- todo make less ugly
