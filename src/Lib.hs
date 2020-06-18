module Lib
    ( someFunc
    )
where

import           Language.Gaiwan
import           Data.List
import           Data.Maybe
import           System.Exit
import           Control.Monad.State.Lazy

someFunc :: IO ()
someFunc = getContents >>= printG . parseGaiwan
  where
    printG (Left m) = do
        putStr "Parsing failed!\n"
        putStr m
        putStr "\n"
        exitFailure
    printG (Right m) = foldr1 (>>) $ convert m

convert (Prog defines main) =
    map (putStr . convertDef) defines
        ++ [putStr $ convertMain (map kindDefs defines) main]


type KindsMap = [(String, [Exp] -> PipelineStep)]

kindDefs :: Stmt -> (String, [Exp] -> PipelineStep)
kindDefs (Shuffler name _ _) = (name, PShuffle name)
kindDefs (Mapper   name _ _) = (name, PMap name)

convertDef (Shuffler name args body) =
    "size_t function user_"
        ++ name
        ++ "("
        ++ intercalate "," (map ("arg_" ++) args)
        ++ "){"
        ++ "return "
        ++ convertBodyB [] body
        ++ ";}\n\n"
convertDef (Mapper name args body) =
    "size_t function user_"
        ++ name
        ++ "("
        ++ intercalate "," (map ("size_t arg_" ++) args)
        ++ "){"
        ++ "return "
        ++ convertBodyB [] body
        ++ ";}\n\n"

fun_prefix True  = "int_"
fun_prefix False = "user_"

convertBody :: KindsMap -> Exp -> String
convertBody km (Let name value body) = error "Let not yet supported!"
convertBody km (Plus a b) = convertBodyB km a ++ "+" ++ convertBodyB km b
convertBody km (Minus a b) = convertBodyB km a ++ "-" ++ convertBodyB km b
convertBody km (Modulo a b) = convertBodyB km a ++ "%" ++ convertBodyB km b
convertBody km (Times a b) = convertBodyB km a ++ "*" ++ convertBodyB km b
convertBody km (Div a b) = convertBodyB km a ++ "/" ++ convertBodyB km b
convertBody km (App f builtin args) =
    fun_prefix builtin
        ++ f
        ++ "("
        ++ intercalate "," (map (convertBodyB km) args)
        ++ ")"
convertBody km (Int num       ) = show num
convertBody km (Var name True ) = "int_" ++ name
convertBody km (Var name False) = "arg_" ++ name
convertBody km (Negate x      ) = "-" ++ convertBodyB km x
convertBody km (ArrayGet x idx) =
    convertBodyB km x ++ "[" ++ convertBody km idx ++ "]"
convertBody km (PipedExp expressions) =
    let pipe = convertPipe km expressions
    in  intercalate "\n\n" $ map (uncurry (convertPls km)) $ zip [1..] pipe

-- convert pipelinestep
convertPls kv i (argIn, argOut@[GPUBuffer outName _], body) =
    "kernel function kernelfun"++show i++"("
        ++ (intercalate ", " $ map
               (\(GPUBuffer name size) ->
                   "size_t " ++ name ++ "[" ++ show size ++ "]"
               )
               (argIn ++ argOut)
           )
        ++ "){ size_t int_index = get_global_id(0);"
        ++ outName
        ++ "[int_index] ="
        ++ (convertBodyB kv body)
        ++ ";}"

-- Add brackets
convertBodyB :: KindsMap -> Exp -> String
convertBodyB km x = "(" ++ convertBody km x ++ ")"

-- Convert main
convertMain defs main = (convertBody defs main) ++ "\n\n"


-- Pipeline stuff
data Pipeline = Pipeline {
        initP :: Exp,
        stepsP :: [PipelineStep]

   }
   deriving (Show)


data Data = Data {
    shuffle:: Exp,
    requiredArrays :: Int,    -- for uniq names
    lastMap :: Bool, -- if the previous step was a map
    exps :: [([GPUBuffer],[GPUBuffer], Exp)],
    curSize :: Int
    }
   deriving (Show)

data PipelineStep = PShuffle String [Exp] | PMap String [Exp]
        deriving (Show)


data GPUBuffer = GPUBuffer String Int deriving (Show)

-- Convert a pipe into kernel specifications
-- todo make datatype with in,out,exp and kernelcode
convertPipe :: KindsMap -> [Exp] -> [([GPUBuffer], [GPUBuffer], Exp)]
convertPipe km (h : r) = reverse $ exps $ foldl convP (convH h) (map getPipeKind r)
  where
    -- convert first item of the pipe
    convH :: Exp -> Data
    convH (App "generateSeq" True [(Int n)]) =
        (Data { shuffle        = Var "index" True
              , requiredArrays = 0
              , lastMap        = True
              , exps = [([], [GPUBuffer "array0" n], Var "index" True)]
              , curSize        = n
              }
        )
    -- converts subsequent steps
    convP x (PShuffle n a) = x
        { shuffle = App n False ((Int (curSize x)) : (shuffle x : a))
        , lastMap = False
        }
    convP x@Data { lastMap = True, exps = (iExp, oExp, cExp) : rExp } (PMap n args)
        = x { exps    = (iExp, oExp, App n False (args ++ [cExp])) : rExp
            , lastMap = True
            }
    convP x@Data { lastMap = False, requiredArrays = ra, shuffle = shuf, exps = (iExp, oExp, cExp) : rExp, curSize = size } (PMap n args)
        = x
            { exps           = ( oExp
                               , [GPUBuffer (arrayName $ ra + 1) size]
                               , App n False $ makeArrayAccess ra shuf : args
                               )
                                   : exps x
            , shuffle        = Var "index" True
            , lastMap        = True
            , requiredArrays = (ra + 1)
            }
    makeArrayAccess :: Int -> Exp -> Exp
    makeArrayAccess name = ArrayGet (Var ("array" ++ show name) True)
    arrayName num = "arrary" ++ show num
    getPipeKind :: Exp -> PipelineStep
    getPipeKind (App name False args) =
        fromMaybe (PMap "err") (lookup name km) args
        -- todo make less ugly
