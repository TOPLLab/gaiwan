module Code
  ( SCode,
    addDeviceCode,
    addHostCode,
    dbgRender,
    execCode,
    getAName,
    lookupDef,
    registerDef,
    runCode,
  )
where

import Control.Monad.State.Lazy
import Data.Functor
import Language.Gaiwan
import OpenCL

data Code = Code
  { deviceCode :: String,
    hostCode :: [OpenCLAction],
    nameCount :: Int,
    defs :: [Stmt],
    kernels :: [(Exp, [GPUBuffer], String)] -- Put kernels here
  }
  deriving (Show)

type SCode a = State Code a

runCode :: Code -> IO ()
runCode c = do
  oclr <- runList (mkOpenRunner (deviceCode c)) (hostCode c)
  str <- showOpenRunner oclr
  putStrLn str
  return ()

dbgRender :: Code -> String
dbgRender c = show (hostCode c) ++ "\n\n" ++ deviceCode c

emptyCode = Code {deviceCode = "", hostCode = [], nameCount = 0, defs = []}

execCode :: SCode a -> Code
execCode s = execState s emptyCode

getAName :: String -> State Code String
getAName prefix = do
  old@Code {nameCount = nc} <- get
  put old {nameCount = nc + 1}
  return $ prefix ++ show nc

registerDef :: Stmt -> SCode ()
registerDef s = modify (\old@Code {defs = d} -> old {defs = s : d})

lookupDef :: String -> SCode (Maybe Stmt)
lookupDef name = get <&> (lookup . defs)
  where
    lookup (r : _) | name == defName r = Just r
    lookup (_ : rest) = lookup rest
    lookup [] = Nothing

defName (Mapper name _ _) = name
defName (Shuffler name _ _) = name

-- todo: add opencl calls
addHostCode :: OpenCLAction -> SCode ()
addHostCode s =
  modify
    ( \old@Code {hostCode = dc} -> old {hostCode = dc ++ [s]}
    )

addDeviceCode :: String -> SCode ()
addDeviceCode s =
  modify
    ( \old@Code {deviceCode = dc} ->
        old {deviceCode = dc ++ "\n\n" ++ s ++ "\n"}
    )

--addDeviceKernel :: Exp -> [GPUBuffer] -> String -> SCode String
--addDeviceKernel exp buffers code = do
--    ks <- kernels <*> get
--    where
--        lookup :: [(Exp, [GPUBuffer], String)] -> Exp -> [GPUBuffer] -> Maybe String
--        lookup ()

