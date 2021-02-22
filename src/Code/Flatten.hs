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
module Code.Flatten
  ( flattenBuffers,
  )
where

import Code.Definitions
import Data.List as L hiding (delete, insert, union)
import Data.Maybe
import Data.Set as S (Set (..), delete, difference, empty, filter, foldr, fromList, insert, lookupMin, member, toList, union)

flattenBuffers :: [GPUAction] -> [GPUAction]
flattenBuffers actions = assignBuffers actionsAndNeed
  where
    -- Transform list of `action` to list of (`action`, buffers needed)
    -- Where buffers needed is a set uf buffers that might be read in the future
    actionsAndNeed = fst $ L.foldr foldrWithNeed ([], S.empty) actions

    foldrWithNeed :: GPUAction -> ([(GPUAction, Set GPUBuffer)], Set GPUBuffer) -> ([(GPUAction, Set GPUBuffer)], Set GPUBuffer)
    foldrWithNeed action@(ReadBuffer buf) (acc, need) =
      let neededBufs = insert buf need
       in ((action, neededBufs) : acc, neededBufs)
    foldrWithNeed action@(CallKernel name usedBuffers writtenBuffers threads) (acc, need) =
      let neededBufs = union need $ fromList usedBuffers
       in ((action, neededBufs) : acc, difference neededBufs (fromList writtenBuffers))

    assignBuffers :: [(GPUAction, Set GPUBuffer)] -> [GPUAction]
    assignBuffers x = reverse $ fst $ L.foldl' assignBufFold ([], (S.empty, [])) x

    assignBufFold ::
      ( [GPUAction], -- collected actions
        ( Set GPUBuffer, -- Free buffers
          [(GPUBuffer, GPUBuffer)] -- Current buffer mapping
        )
      ) ->
      (GPUAction, Set GPUBuffer) ->
      ([GPUAction], (Set GPUBuffer, [(GPUBuffer, GPUBuffer)]))
    assignBufFold (acc, (free, mapping)) (action, needed) =
      let a = map (\x -> (x, lookup x mapping)) $ toList needed
       in let (free1, mapping1) = L.foldr assign (free, mapping) a
           in let (stillNeeded, notNeeded) = partition (\(x, _) -> member x needed) mapping1
               in ( translate mapping1 action : acc,
                    ( union free1 $ fromList $ map snd notNeeded,
                      stillNeeded
                    )
                  )

    -- Apply a mappign of GPUBuffers to a GPU action
    -- Assumes that all mentioned GPU buffers are assigned in the mapping
    translate :: [(GPUBuffer, GPUBuffer)] -> GPUAction -> GPUAction
    translate m (ReadBuffer buf) =
      ReadBuffer $ justLookup m buf
    translate m (CallKernel name usedBuffers writtenBuffers threads) =
      CallKernel
        name
        (map (justLookup m) usedBuffers)
        (map (justLookup m) writtenBuffers)
        threads

    -- Assign
    -- returns (currently free buffers, current lookup table)
    assign :: (GPUBuffer, Maybe GPUBuffer) -> (Set GPUBuffer, [(GPUBuffer, GPUBuffer)]) -> (Set GPUBuffer, [(GPUBuffer, GPUBuffer)])
    assign (_, Just _) (free, table) = (free, table) -- do nothing if found
    assign (ins@(GPUBuffer name size), Nothing) (free, table) =
      case lookupMin $ S.filter (\(GPUBuffer _ s) -> s == size) free of
        Just b -> (delete b free, (ins, b) : table) -- use if free availible
        Nothing -> (free, (ins, ins) : table) -- new if not

    -- Lookup from just (Jou must be able to prove that the value will be in the list)
    justLookup m v = fromJust $ lookup v m
