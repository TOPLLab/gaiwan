{-# LANGUAGE RankNTypes #-}

module Code
  ( SCode,
    KernelName (),
    GPUAction (CallKernel, ReadBuffer), -- do not export AllocBuffer
    GPUBuffer (..),
    addDeviceCode,
    addDeviceKernel,
    compile,
    compileUnopt,
    addHostCode,
    execCode,
    lookupDef,
    registerDef,
    freshGPUBuffer,
    runCodeToList,
    gpuBufferSize,
    serialize,
    runCompiled,
  )
where

import Code.Definitions as C
import Code.Flatten
import Code.SCode
import Code.Serialize
import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import Data.List as L hiding (delete, insert, union)
import Data.Maybe
import Data.Set as S (Set (..), delete, difference, empty, filter, foldr, fromList, insert, lookupMin, member, toList, union)
import Language.Gaiwan
import Language.GaiwanDefs
import OpenCL

compile :: Code a -> (a, [GPUAction])
compile c = (deviceCode c, bufAlloc ++ prog)
  where
    prog = flattenBuffers $ hostCode c
    bufAlloc = C.AllocBuffer <$> toList (collectBuffers prog)
    collectBuffers ((CallKernel _ bufs _ _) : r) = S.union (collectBuffers r) $ fromList bufs
    collectBuffers (_ : r) = collectBuffers r
    collectBuffers [] = empty

compileUnopt :: Code a -> (a, [GPUAction])
compileUnopt c = (deviceCode c, prog) -- TODO add buffAlloc back TODO clean
  where
    prog = hostCode c
    bufAlloc = C.AllocBuffer <$> toList (collectBuffers prog)
    collectBuffers ((CallKernel _ bufs _ _) : r) = S.union (collectBuffers r) $ fromList bufs
    collectBuffers (_ : r) = collectBuffers r
    collectBuffers [] = empty

toOpenCL :: GPUAction -> OpenCLAction
toOpenCL (CallKernel (KernelName n) args _ threads) = MakeKernel n (map toOpenCLBuf args) (Range threads 0 0)
toOpenCL (C.ReadBuffer x) = OpenCL.ReadBuffer (toOpenCLBuf x)
toOpenCL (C.AllocBuffer b) = OpenCL.AllocBuffer $ toOpenCLBuf b

toOpenCLBuf (GPUBuffer (GPUBufferName i) size) = CLGPUBuffer i size

runCodeToList :: Code String -> IO [[Integer]]
runCodeToList c = uncurry runToList (compile c)

runCompiled :: BS.ByteString -> IO (Maybe [[Integer]])
runCompiled s =
  maybe
    (return Nothing)
    (fmap Just . uncurry runToList)
    $ deserialize s

runToList :: String -> [GPUAction] -> IO [[Integer]]
runToList devCode hostCode = do
  runner <- mkOpenRunnerInteger devCode
  run runner $ addFrees $ map toOpenCL hostCode
  where
    addFrees l =
      l
        ++ L.foldr
               ( \new a -> case new of
                   OpenCL.AllocBuffer x -> OpenCL.FreeBuffer x:a
                   _ -> a
               )
               []
               l

-- Adds a kernel and retrns the name
-- Creates a kernel that sets the output of the i-th expression to the i-th output buffer
addDeviceKernel ::
  Monoid a =>
  ( (Exp -> SCode a b) ->
    (KernelName -> [GPUBuffer] -> [GPUBuffer] -> [b] -> a) ->
    [Exp] ->
    [GPUBuffer] ->
    [GPUBuffer] ->
    SCode a KernelName
  )
addDeviceKernel mkCode mkKernelShell initExps initBuffers initBuffersout = do
  ks <- getKernels
  maybe realyAddKernel return $ lookup ks
  where
    (exps, buffers, buffersout) = canonicalKernel (initExps, initBuffers, initBuffersout)
    matches e = (exps, buffers, buffersout) == e -- ignores names because previous line
    lookup :: [(([Exp], [GPUBuffer], [GPUBuffer]), KernelName)] -> Maybe KernelName
    lookup ((h, n) : r) | matches h = Just n
    lookup (_ : r) = lookup r
    lookup [] = Nothing

    realyAddKernel = do
      name <- freshKernelName
      code <- mapM mkCode exps
      addDeviceCode $ mkKernelShell name buffers buffersout code
      registerKernel exps buffers buffersout name -- remember for next time
      return name

canonicalKernel :: ([Exp], [GPUBuffer], [GPUBuffer]) -> ([Exp], [GPUBuffer], [GPUBuffer])
canonicalKernel (e, buffers, buffersOut) =
  (map (substGPUBuffers canonicalBuffers) e, translatedBuffers, translatedBuffersOut)
  where
    canonicalBuffers = canonicalBufferKV $ buffers ++ buffersOut
    translatedBuffers = map translateBuffer buffers
    translatedBuffersOut = map translateBuffer buffersOut
    translateBuffer x = fromJust $ lookup x canonicalBuffers

canonicalBufferKV :: [GPUBuffer] -> [(GPUBuffer, GPUBuffer)]
canonicalBufferKV buffers = removeDoubles $ zip buffers $ canonicalBufferNames buffers

removeDoubles [] = []
removeDoubles ((k, v) : r) = (k, v) : removeDoubles (L.filter (\(ko, _) -> ko /= k) r)

canonicalBufferNames :: [GPUBuffer] -> [GPUBuffer]
canonicalBufferNames buffers = zipWith (\a@(GPUBuffer _ s) newname -> GPUBuffer newname s) buffers $ GPUBufferName <$> [1000 ..]
