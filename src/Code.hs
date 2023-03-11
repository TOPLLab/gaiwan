{-# LANGUAGE RankNTypes #-}

module Code
  ( SCode,
    KernelName (),
    GPUAction (CallKernel, ReadBuffer, OutputBuffer), -- do not export AllocBuffer
    ReservedBuffer (..),
    addDeviceCode,
    addDeviceKernel,
    addDeviceReducerKernel,
    addDeviceAssocReducerKernel,
    addHostReadBuffer,
    compile,
    addHostCode,
    execCode,
    lookupDef,
    registerDef,
    freshGPUBuffer,
    -- deserialize,
    -- serialize,
  )
where

import Code.Definitions as C
import Code.Flatten
import Code.SCode
-- import Code.Serialize
import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BS
import Data.Functor
import Data.List as L hiding (delete, insert, union)
import Data.Maybe
import Data.Set as S (Set (..), delete, difference, empty, filter, foldr, fromList, insert, lookupMin, member, toList, union)
import Language.Gaiwan
import Language.GaiwanDefs

compile :: Code a -> (a, [GPUAction])
compile c = (deviceCode c, bufAlloc ++ prog)
  where
    prog = flattenBuffers $ hostCode c
    bufAlloc = C.AllocBuffer <$> toList (collectBuffers prog)
    collectBuffers ((CallKernel _ inBufs outBufs) : r) = S.union (collectBuffers r) $ fromList outBufs
    collectBuffers ((CallReducerKernel name usedBuffers writtenBuffer) : r) = S.insert writtenBuffer (collectBuffers r)
    collectBuffers ((CallAssocReducerKernel _ _ usedBuffers writtenBuffer) : r) = S.insert writtenBuffer (collectBuffers r)
    -- collectBuffers ((ReadBuffer s rb) : r) = S.union (collectBuffers r) $ fromList [rb]
    -- collectBuffers ((OutputBuffer rbs) : r) = S.union (collectBuffers r) $ fromList rbs
    collectBuffers (_ : r) = collectBuffers r
    collectBuffers [] = empty

-- Adds a kernel and retrns the name
-- Creates a kernel that sets the output of the i-th expression to the i-th output buffer
-- TODO remove duplicates
addDeviceKernel ::
  Monoid a =>
  ( (BExp -> SCode a b) ->
    (KernelName -> [ReservedBuffer] -> [ReservedBuffer] -> [b] -> a) ->
    [BExp] ->
    [ReservedBuffer] ->
    [ReservedBuffer] ->
    SCode a KernelName
  )
addDeviceKernel mkCode mkKernelShell exps buffers buffersout = do
  name <- freshKernelName
  code <- mapM mkCode exps
  addDeviceCode $ mkKernelShell name buffers buffersout code
  registerKernel exps buffers buffersout name -- remember for next time
  return name

addDeviceReducerKernel ::
  Monoid a =>
  ( (BExp -> SCode a b) ->
    (KernelName -> [ReservedBuffer] -> ReservedBuffer -> b -> a) ->
    BExp ->
    [ReservedBuffer] ->
    ReservedBuffer ->
    SCode a KernelName
  )
addDeviceReducerKernel mkCode mkKernelShell exp buffers bufferout = do
  name <- freshKernelName
  code <- mkCode exp
  addDeviceCode $ mkKernelShell name buffers bufferout code
  registerKernel [exp] buffers [bufferout] name -- remember for next time TODO remove?
  return name

-- Separate function, because 2 kernels are created
addDeviceAssocReducerKernel ::
  Monoid a =>
  ( (BExp -> SCode a b) ->
    (KernelName -> KernelName -> [ReservedBuffer] -> ReservedBuffer -> b -> b -> a) ->
    BExp -> -- initial acc phase (combining 2 (mapped) values)
    BExp -> -- tree acc phase combing 2 results
    [ReservedBuffer] ->
    ReservedBuffer ->
    SCode a (KernelName, KernelName)
  )
addDeviceAssocReducerKernel mkCode mkKernelShell exp1 exp2 buffers bufferout = do
  name1 <- freshKernelName
  name2 <- freshKernelName
  code1 <- mkCode exp1
  code2 <- mkCode exp2
  addDeviceCode $ mkKernelShell name1 name2 buffers bufferout code1 code2
  registerKernel [exp1] buffers [bufferout] name1 -- remember for next time TODO remove?
  registerKernel [exp2] [bufferout] [bufferout] name2 -- remember for next time TODO remove?
  return (name1, name2)
