{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module CodeGen.Pipelining (prepare, makePlan) where

-- The idea is to transform a list of shuffles and maps into a list of maps (containing array)

import Code
import Code.Definitions
import CodeGen.CLike
import Control.Lens
import Control.Monad
import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import Debug.Trace
import Language.GaiwanDefs
import Language.GaiwanTypes

data PipelineStep = PipelineStep [(GaiwanBuf Int, BExp)] -- [(buffer to write data to, what to write to it)]
  deriving (Show)

type TmpCode a = SCode String a

prepare :: TypedProgram -> (String, [GPUAction])
prepare p = compile $ execCode $ makePlanning p

-- helper function for only testing the plan
makePlan :: TypedProgram -> [GPUAction]
makePlan p = (\x -> (Infoz (fst x) : (snd x))) $ prepare p

makePlanning :: TypedProgram -> TmpCode ()
makePlanning (TypedProg (GTransformType contraints fromT toT) actions) =
  do
    da <- foldlM processActions emptyPlan actions
    resultBuffers <- toKernel da
    addHostCode $ (OutputBuffer resultBuffers)
    return ()

toKernel :: PlanData -> TmpCode [ReservedBuffer]
toKernel (PipelineStep results) = do
  let argBufs = Set.toList $ getBuffers (map snd results)
  outBuf <- mapM freshGPUBuffer (map fst results)
  fName <- addDeviceKernel mkCode kernelTemplate (map snd results) argBufs outBuf
  addHostCode $ CallKernel fName argBufs outBuf
  return outBuf

gpuBufferSize :: ReservedBuffer -> GaiwanBufSize Int
gpuBufferSize (ReservedBuffer _ (GaiwanBuf gbs _)) = gbs

getBuffers :: [BExp] -> Set.Set ReservedBuffer
getBuffers = foldr addUsed Set.empty
  where
    addUsed :: BExp -> Set.Set ReservedBuffer -> Set.Set ReservedBuffer
    addUsed (Let s ge' ge2) = addUsed ge' . addUsed ge2
    addUsed (Plus ge' ge2) = addUsed ge' . addUsed ge2
    addUsed (Minus ge' ge2) = addUsed ge' . addUsed ge2
    addUsed (Modulo ge' ge2) = addUsed ge' . addUsed ge2
    addUsed (Times ge' ge2) = addUsed ge' . addUsed ge2
    addUsed (Pow ge' ge2) = addUsed ge' . addUsed ge2
    addUsed (Div ge' ge2) = addUsed ge' . addUsed ge2
    addUsed (Int n) = id
    addUsed (Tuple ges) = Set.union (getBuffers ges)
    addUsed (Select ge' n) = addUsed ge'
    addUsed (Var s b) = id
    addUsed (Negate ge') = addUsed ge'
    addUsed (ArrayGet ge' ge2) = error "Shoud not occur"
    addUsed (GPUBufferGet rb ge') = Set.insert rb . addUsed ge'
    addUsed (If ge' ge2 ge3) = addUsed ge' . addUsed ge2 . addUsed ge3
    addUsed (IsEq ge' ge2) = addUsed ge' . addUsed ge2
    addUsed (IsGreater ge' ge2) = addUsed ge' . addUsed ge2

type PlanData = PipelineStep

-- Substiute a for b in c (with instructions)
substTI :: VarSpecifier -> Exp -> TypedInstr -> TypedInstr
substTI from to = substMultTI [(from, to)]

--
-- Don't map the ts of app
substMultTI :: [(VarSpecifier, Exp)] -> TypedInstr -> TypedInstr
substMultTI kv (TLoop st cnt varname instrs) = TLoop st (simplifyExp $ substMult kv cnt) varname (map (substMultTI newKv) instrs)
  where
    newKv = filter (\((k, False), _) -> k /= varname) kv
substMultTI kv (TIApp t ts args) = TIApp t ts (map (simpleSubstMult kv) args)

emptyPlan :: PlanData
emptyPlan = PipelineStep []

-- TODO check arg length at typechecking

readToAccess :: ReservedBuffer -> (GaiwanBuf Int, BExp)
readToAccess a@(ReservedBuffer gbn gb) = (gb, (GPUBufferGet a theI))

processActions :: PlanData -> TypedInstr -> TmpCode PlanData
processActions pd@(PipelineStep b) (TReturn (GTransformType contrainst fT tT) buffers) | length b == 0 = do
  let nameBuffer = zip buffers tT
  readBuffers <- mapM (uncurry addHostReadBuffer) nameBuffer
  return $ PipelineStep (map readToAccess readBuffers)
processActions pd (TIApp zz (TAbstraction t _ argnames body) argvalues) | length argnames == length argvalues =
  do
    let kv = zip argnames argvalues
    let appliedBody = map (substMultStmt kv) body
    foldM processApplicationD pd appliedBody
processActions foldData (TLoop _ (Int cnt) varname steps) =
  foldM
    processActions
    foldData
    $ concatMap (\i -> map (substTI (varname, False) (Int i)) steps) [0 .. (cnt - 1)]
processActions _ e = error $ "help " ++ show e

substMultStmt :: [(String, Exp)] -> TypedTransform -> TypedTransform
substMultStmt kv (TShaper t name args exp) = TShaper t name args (substExcept kv args exp)
substMultStmt kv (TMapper t name args exp) = TMapper t name args (substExcept kv args exp)
substMultStmt kv (TReducer t name args init exp) = TReducer t name args (substExcept kv args init) (substExcept kv args exp)

substExcept :: [(String, Exp)] -> [String] -> Exp -> Exp
substExcept kv args = simpleSubstMult (kvExcept args kv)

kvExcept :: [String] -> [(String, Exp)] -> [(VarSpecifier, Exp)]
kvExcept args kv = map (\(k, vv) -> ((k, False), vv)) $ kvExceptBase args kv

kvExceptBase :: Eq a => [a] -> [(a, b)] -> [(a, b)]
kvExceptBase args = filter (\(k, _) -> k `notElem` args)

theI = Var "i" True

substByTheI :: String -> BExp -> BExp
substByTheI x = simpleSubst (x, False) theI

substTheIBy a b = simplifyExp $ subst ("i", True) a b

toBExp :: Exp -> BExp
toBExp e = mapExp (const Nothing) e

traceThis x a = a

-- TODO check for duplicates argument names in typing
processApplication :: PlanData -> TypedTransform -> TmpCode PlanData
processApplication (PipelineStep inputs) (TShaper (GTransformType contraints fromT [toT]) name (iname : bufferArgs) exp) | length inputs == length bufferArgs && quiteSimple inputs = do
  --addHostCode $ Infoz $ "add call to shaper " ++ name ++ "(XXX)"
  return $ PipelineStep [(toT, foldr f (substByTheI iname (toBExp exp)) (zip bufferArgs inputs))]
  where
    f (argName, (_, buf)) expy = traceThis "substArrrayGet ->" $ substArrayGet argName (g buf) (traceThis "Input to subst =>" expy)

    g :: BExp -> BExp -> BExp
    g realArrayBufExp usedIndexInShpr = traceThis "OKKKKKKKKKKKKKKKKKKKK" $ substTheIBy (traceThis "AAAAAAAAA" usedIndexInShpr) (traceThis "BBBBBBBB" realArrayBufExp)
processApplication complex@(PipelineStep inputs) task@(TShaper (GTransformType contraints fromT [toT]) name (iname : bufferArgs) exp) | length inputs == length bufferArgs = do
  resBuffers <- toKernel complex
  processApplication (PipelineStep (map readToAccess resBuffers)) task
processApplication (PipelineStep [(_, input)]) (TMapper (GTransformType contraints [fromT] [toT]) name [iname, dname] exp) = do
  -- addHostCode $ Infoz $ "add call to  " ++ name ++ "()"
  return $ PipelineStep [(toT, simpleSubstMult [((iname, False), theI), ((dname, False), input)] (toBExp exp))]
processApplication _ a = error $ show a -- TODO: handle reducer

quiteSimple :: [(GaiwanBuf Int, BExp)] -> Bool
quiteSimple s
  | ( all
        ( \(GaiwanBuf _ t, _) -> case t of
            GaiwanInt -> False
            (GaiwanTuple gss) -> True
            (TVar any) -> error "Cannot handle this yet..."
        )
        s
    ) =
    True
quiteSimple input = (sum $ (map (complexity . snd) input)) < 40

--processApplication pd (TAbstraction t _ [] body) =
--  foldM processApplicationD pd body

processApplicationD a b = do
  x <- processApplication a b
  -- addHostCode $ Infoz $ show x
  return x
