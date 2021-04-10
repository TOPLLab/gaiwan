module CodeGen.Render where

import Code
import Code.Definitions
import Code.SCode
import qualified CodeGen.CLike as CLike
import CodeGen.Pipelining
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.Maybe
import Language.GaiwanDefs
import RenderDefs

type BufferLookup = PicBufferLookup GPUBuffer

type KernelWrapper a = [(GPUBuffer, PicBuffer)] -> Int -> a

(>->) :: SPic (KernelWrapper a) -> (a -> b) -> SPic (KernelWrapper b)
(>->) x f = haha <$> x
  where
    haha g lt i = f $ g lt i

(>>>) :: SPic (KernelWrapper a) -> ([(GPUBuffer, PicBuffer)] -> Int -> a -> b) -> SPic (KernelWrapper b)
(>>>) x f = do
  hah <$> x
  where
    hah thea lt i = f lt i (thea lt i)

type KernelType = KernelWrapper (Int, [(PicBuffer, Int)])

type KernelPicType = [GPUBuffer] -> State BufferLookup PicLevel

type KernelPics = [(KernelName, KernelPicType)]

type SPic a = SCode KernelPics a

mkCode :: Exp -> SPic ()
mkCode (PipedExp actions) = convertPls False mkCode2 mkKernel actions

mkBinOp :: (Int -> Int -> Int) -> Exp -> Exp -> SPic KernelType
mkBinOp op a b = do
  af <- mkCode2 a
  bf <- mkCode2 b
  return $ retval af bf
  where
    retval af bf lt i =
      ( op a1 b1, -- do binop
        a2 ++ b2 -- join required arrays
      )
      where
        (a1, a2) = af lt i
        (b1, b2) = bf lt i

mkBoolBinOp :: (Int -> Int -> Bool) -> Exp -> Exp -> SPic KernelType
mkBoolBinOp f = mkBinOp (\x y -> if f x y then 1 else 0)

-- Compute values
mkCode2 :: Exp -> SPic KernelType
mkCode2 (Plus a b) = mkBinOp (+) a b
mkCode2 (Minus a b) = mkBinOp (+) a b
mkCode2 (Times a b) = mkBinOp (*) a b
mkCode2 (Div a b) = mkBinOp div a b
mkCode2 (Modulo a b) = mkBinOp mod a b
mkCode2 (IsGreater a b) = mkBoolBinOp (>) a b
mkCode2 (IsEq a b) = mkBoolBinOp (==) a b
mkCode2 (Negate a) = mkCode2 a >-> (\(v, b) -> (- v, b))
mkCode2 (Int n) = return $ const . const (n, [])
mkCode2 (Var "index" True) = return $ \lt i -> (i, [])
mkCode2 (If cond tExp fExp) = do
  f <- mkCode2 fExp
  t <- mkCode2 tExp
  mkCode2 cond >>> \lt i (cond, buffers) ->
    (\(e, b) -> (e, b ++ buffers))
      ( case cond of
          0 -> f lt i
          _ -> t lt i
      )
mkCode2 (GPUBufferGet buffer idx) = do
  mkCode2 idx
    >>> \lt _ (index, buffers) ->
      let actualBuffer = fromJust (lookup buffer lt)
       in (picBufferIndex actualBuffer index, (actualBuffer, index) : buffers)
mkCode2 t = error $ show t

assert :: Bool -> a -> a
assert True = id
assert False = \_ -> error "Assertion Failed" -- const (error "Assertion Failed")

mkKernel :: KernelName -> [GPUBuffer] -> [GPUBuffer] -> [KernelType] -> KernelPics
mkKernel name kernelBuffers outBuffers code = [(name, _mkKernel)]
  where
    _mkKernel :: [GPUBuffer] -> State BufferLookup PicLevel
    _mkKernel actualBuffers = do
      ltb <- renamePictBuffer (zip actualBuffers kernelBuffers) <$> get
      hahaha <-
        mapM
          ( \(gpub@(GPUBuffer _ size), f) -> -- realBuffer, codefunc
              addPictBuffer gpub $
                map
                  ( \i ->
                      let (val, src) = f ltb i
                       in PicElement (map (first picBufferId) src) val
                  )
                  [0 .. size -1]
          )
          ( zip
              ( map (\b -> fromJust $ lookup b (zip kernelBuffers actualBuffers)) outBuffers
              -- Hier actual outbuffers oplijsten
              )
              code
          )
      jjjj <- get
      return $ PicLevel (show name) hahaha

render :: Program -> Pic
render (Prog defines prog) = Pic foldProg
  where
    (kernels, actions) = compile . execCode $ do
      mapM_ registerDef defines
      mkCode prog
    foldProg :: [PicLevel]
    foldProg = reverse $ evalState (foldM foldf [] actions) empty
      where
        lookupKenel :: KernelName -> KernelPicType
        lookupKenel n = fromJust $ lookup n kernels

        foldf :: [PicLevel] -> GPUAction -> State BufferLookup [PicLevel]
        foldf l (AllocBuffer buffer) = return l -- ignore
        foldf l (ReadBuffer _) = return l -- ignore
        foldf l (CallKernel name inBuf _ _) = do
          b <- lookupKenel name inBuf
          return $ b : l
