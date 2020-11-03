module Code
    ( Code
    , execCode
    , getAName
    , addDeviceCode
    , addHostCode
    , dbgRender
    , runCode
    )
where
import           Control.Monad.State.Lazy
import           OpenCL
data Code = Code {
    deviceCode :: String,
    hostCode :: [OpenCLAction],
    nameCount :: Int
} deriving (Show)

runCode :: Code -> IO ()
runCode c = do
    oclr <- runList (mkOpenRunner (deviceCode c)) (hostCode c)
    str <- showOpenRunner oclr
    putStrLn str
    return ()

dbgRender :: Code -> String
dbgRender c = show (hostCode c) ++ "\n\n" ++ deviceCode c

emptyCode = Code { deviceCode = "", hostCode = [], nameCount = 0 }

execCode :: State Code a -> Code
execCode s = execState s emptyCode

getAName :: String -> State Code String
getAName prefix = do
    old@Code { nameCount = nc } <- get
    put old { nameCount = nc + 1 }
    return $ prefix ++ show nc



-- todo: add opencl calls
addHostCode :: OpenCLAction -> State Code ()
addHostCode s = modify
    (\old@Code { hostCode = dc } -> old { hostCode = dc ++ [s]}
    )

addDeviceCode :: String -> State Code ()
addDeviceCode s = modify
    (\old@Code { deviceCode = dc } ->
        old { deviceCode = dc ++ "\n\n" ++ s ++ "\n" }
    )
