module Code
  ( SCode,
    KernelName (),
    GPUAction (CallKernel, ReadBuffer), -- do not export AllocBuffer
    GPUBuffer (..),
    addDeviceCode,
    addDeviceKernel,
    compile,
    addHostCode,
    gpuBufferGet,
    dbgRender,
    execCode,
    lookupDef,
    registerDef,
    freshGPUBuffer,
    runCodeToList,
    gpuBufferSize,
  )
where

import Code.Definitions as C
import Code.Flatten
import Code.SCode
import Data.Bifunctor
import Data.Functor
import Data.List as L hiding (delete, insert, union)
import Data.Maybe
import Data.Set as S (Set (..), delete, difference, empty, filter, foldr, fromList, insert, lookupMin, member, toList, union)
import Debug.Trace
import Language.Gaiwan
import Language.GaiwanDefs
import OpenCL

compile :: Code -> (String, [GPUAction])
compile c = (deviceCodeStr c, bufAlloc ++ prog)
  where
    prog = flattenBuffers $ hostCode c
    bufAlloc = C.AllocBuffer <$> toList (collectBuffers prog)
    collectBuffers ((CallKernel _ bufs _ _) : r) = S.union (collectBuffers r) $ fromList bufs
    collectBuffers (_ : r) = collectBuffers r
    collectBuffers [] = empty

toOpenCL :: GPUAction -> OpenCLAction
toOpenCL (CallKernel (KernelName n) args _ threads) = MakeKernel n (map toOpenCLBuf args) (Range threads 0 0)
toOpenCL (C.ReadBuffer x) = OpenCL.ReadBuffer (toOpenCLBuf x)
toOpenCL (C.AllocBuffer b) = OpenCL.AllocBuffer $ toOpenCLBuf b

toOpenCLBuf (GPUBuffer (GPUBufferName i) size) = CLGPUBuffer i size

runCodeToList :: Code -> IO [[Integer]]
runCodeToList c = do
  runner <- mkOpenRunnerInteger devCode
  run runner $ map toOpenCL hostCode
  where
    (devCode, hostCode) = compile c

dbgRender :: Code -> String
dbgRender c = show (hostCode c) ++ "\n\n" ++ deviceCodeStr c

-- Adds a kernel and retrns the name
-- Creates a kernel that sets the output of the i-th expression to the i-th output buffer
addDeviceKernel :: (Exp -> SCode String) -> [Exp] -> [GPUBuffer] -> [GPUBuffer] -> SCode KernelName
addDeviceKernel mkCode initExps initBuffers initBuffersout = do
  ks <- getKernels
  maybe realyAddKernel return $ lookup ks
  where
    (exps, buffers, buffersout) = canonicalKernel (initExps, initBuffers, initBuffersout)
    matches e = (exps, buffers, buffersout) == e -- ignores names because previous line

    lookup :: [(([Exp], [GPUBuffer], [GPUBuffer]), KernelName)] -> Maybe KernelName
    lookup ((h, n) : r) | matches h = Just n
    lookup (_ : r) = lookup r
    lookup [] = Nothing

    realyAddKernel :: SCode KernelName
    realyAddKernel = do
      name <- freshKernelName
      code <- mapM mkCode exps
      addDeviceCode $
        mkKernelShell name buffers $
          " int int_index = get_global_id(0);\n"
            ++ intercalate "\n" (zipWith (gpuBufferAssign "int_index") buffersout code)
      registerKernel exps buffers buffersout name -- remember for next time
      return name

canonicalKernel :: ([Exp], [GPUBuffer], [GPUBuffer]) -> ([Exp], [GPUBuffer], [GPUBuffer])
canonicalKernel (e, buffers, buffersOut) = (map (simpleSubstMult requiredSubst) e, translatedBuffers, translatedBuffersOut)
  where
    canonicalBuffers = canonicalBufferKV $ buffers ++ buffersOut
    translatedBuffers = map translateBuffer buffers
    translatedBuffersOut = map translateBuffer buffersOut
    translateBuffer x = fromJust $ lookup x canonicalBuffers
    requiredSubst :: [(Exp, Exp)]
    requiredSubst = map (bimap gpuBufferVar gpuBufferVar) canonicalBuffers

canonicalBufferKV :: [GPUBuffer] -> [(GPUBuffer, GPUBuffer)]
canonicalBufferKV buffers = removeDoubles $ zip buffers $ canonicalBufferNames buffers

removeDoubles [] = []
removeDoubles ((k, v) : r) = (k, v) : removeDoubles (L.filter (\(ko, _) -> ko /= k) r)

canonicalBufferNames :: [GPUBuffer] -> [GPUBuffer]
canonicalBufferNames buffers = zipWith (\a@(GPUBuffer _ s) newname -> GPUBuffer newname s) buffers $ GPUBufferName <$> [0 ..]

mkKernelShell :: KernelName -> [GPUBuffer] -> String -> String
mkKernelShell (KernelName name) args code = "void kernel " ++ name ++ "(" ++ argsStr ++ ")" ++ "{ \n" ++ code ++ " \n};"
  where
    argsStr = intercalate ", " (map gpuBufferDecl args)


gpuBufferDecl gpub@(GPUBuffer _ size) =
  "global int " ++ gpuBufferArgName gpub ++ "[" ++ show size ++ "]"

gpuBufferAssign :: String -> GPUBuffer -> String -> String
gpuBufferAssign index buffer value = gpuBufferArgName buffer ++ "[" ++ index ++ "] = " ++ value ++ ";"

-- TODO make the stuff below nicer
gpuBufferGet :: GPUBuffer -> Exp -> Exp
gpuBufferGet buffer = ArrayGet (gpuBufferVar buffer)

gpuBufferVar (GPUBuffer (GPUBufferName name) _) = Var ("array" ++ show name) True

gpuBufferArgName (GPUBuffer (GPUBufferName name) _) = "int_array" ++ show name
