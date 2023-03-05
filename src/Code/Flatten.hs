--
-- Reuse GPUBuffers
--
-- We look at all the actions that are carried out and look at the required
-- buffers and their sizes. With a backwards traversal we determine what buffers
-- are needed. A subsequnect forward traversal only creates a new buffer if there
-- is not already a allocated and unused buffer.
--
-- This could be improved further by using graph colouring techniques, but to keep
-- things simple, we use this simple method.
--
--
--
-- TODO: document that all ReadBuffer's of the same buffer will be read only,
-- And therefore best all use the same GaiwanBuf if they correspond to the same read buf
--
module Code.Flatten
  ( flattenBuffers,
  )
where

import Code.Definitions
import Data.List as L hiding (delete, insert, union)
import Data.Maybe
import Data.Set as S (Set (..), delete, difference, empty, filter, foldr, fromList, insert, lookupMin, member, toList, union)
import Debug.Trace

flattenBuffers :: [GPUAction] -> [GPUAction]
flattenBuffers actions = assignBuffers actionsAndNeed
  where
    -- Transform list of `action` to list of (`action`, buffers needed)
    -- Where buffers needed is a set of buffers that might be read in the future
    actionsAndNeed = fst $ L.foldr foldrWithNeed ([], readBuffers actions) actions

    readBuffers :: [GPUAction] -> Set ReservedBuffer
    readBuffers e = L.foldr (\x m -> case x of (ReadBuffer _ rb) -> S.insert rb m; _ -> m) S.empty e

    foldrWithNeed ::
      GPUAction -> -- current Action
      ( [(GPUAction, Set ReservedBuffer)], -- Next actions and the buffers they need themselves (for reading or writing)
        Set ReservedBuffer -- Buffers whose value are needed by future actions (for reading)
      ) ->
      ([(GPUAction, Set ReservedBuffer)], Set ReservedBuffer)
    foldrWithNeed action@(OutputBuffer buffers) (acc, need) =
      let neededBufs = union need (fromList buffers)
       in ((action, neededBufs) : acc, neededBufs)
    foldrWithNeed action@(CallKernel name usedBuffers writtenBuffers) (acc, need) =
      let neededBufs = union need $ fromList usedBuffers
       in ((action, need) : acc, difference neededBufs (fromList writtenBuffers))
    foldrWithNeed action (acc, need) = ((action, need) : acc, need)
    assignBuffers :: [(GPUAction, Set ReservedBuffer)] -> [GPUAction]
    assignBuffers x = reverse $ fst $ L.foldl' assignBufFold ([], (S.empty, [])) x

    assignBufFold ::
      ( [GPUAction], -- collected actions
        ( Set ReservedBuffer, -- Free buffers
          [(ReservedBuffer, ReservedBuffer)] -- Current buffer mapping
        )
      ) ->
      (GPUAction, Set ReservedBuffer) ->
      ([GPUAction], (Set ReservedBuffer, [(ReservedBuffer, ReservedBuffer)]))
    assignBufFold (acc, (free, mapping)) (action, needed) =
      let a = map (\x -> (x, lookup x mapping)) $ toList needed
       in let (free1, mapping1) = L.foldr assign (free, mapping) a
           in let (stillNeeded, notNeeded) = partition (\(x, _) -> member x needed) mapping1
               in let reworkedAction = translate mapping1 action
                   in ( reworkedAction : acc,
                        ( union free1 $ fromList $ map snd notNeeded,
                          stillNeeded
                        )
                      )

    -- Apply a mapping of GPUBuffers to a GPU action
    -- Assumes that all mentioned GPU buffers are assigned in the mapping
    translate :: [(ReservedBuffer, ReservedBuffer)] -> GPUAction -> GPUAction
    translate m r@(ReadBuffer {}) = r
    translate m (CallKernel name usedBuffers writtenBuffer) =
      CallKernel
        name
        (map (justLookup m) usedBuffers)
        (map (justLookup m) writtenBuffer)
    translate m (AllocBuffer {}) = error "Flatten should not be used if there are already allocated buffers!"
    translate m (OutputBuffer buffers) = OutputBuffer (map (justLookup m) buffers)
    translate m x@(Infoz {}) = x

    -- Assign
    -- returns (currently free buffers, current lookup table)
    assign ::
      (ReservedBuffer, Maybe ReservedBuffer) ->
      ( Set ReservedBuffer, -- currently free
        [(ReservedBuffer, ReservedBuffer)] -- mappings made
      ) ->
      ( Set ReservedBuffer, -- new free
        [(ReservedBuffer, ReservedBuffer)] -- new mapping
      )
    assign (_, Just _) (free, table) = (free, table) -- do nothing if found
    assign (ins@(ReservedBuffer name shapeSpecifier), Nothing) (free, table) =
      case lookupMin $ S.filter (\(ReservedBuffer _ b) -> b == shapeSpecifier) free of
        Just b -> (delete b free, (ins, b) : table) -- use if free availible
        Nothing -> (free, (ins, ins) : table) -- new if not

    -- Lookup from just (Jou must be able to prove that the value will be in the list)
    justLookup m v = fromJust $ lookup v m
