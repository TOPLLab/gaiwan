{-# LANGUAGE OverloadedStrings #-}

module RenderDefs
  ( Pic (..),
    PicLevel (..),
    PicBuffer (..),
    PicElement (..),
    PicBufferId (), -- hiden constructor
    picBufferId,
    picBufferIndex,
    PicBufferLookup (),
    renamePictBuffer,
    empty,
    addPictBuffer,
    asJSON,
  )
where

import Control.Monad.State.Lazy
import Data.Aeson hiding (value)
import Data.Bifunctor
import qualified Data.ByteString.Lazy as BS
import Data.Maybe

instance Monoid Pic where
  mempty = Pic []

instance Semigroup Pic where
  (<>) (Pic a) (Pic b) = Pic (a ++ b)

newtype PicBufferId = PicBufferId Int deriving (Eq, Show)

newtype Pic = Pic [PicLevel] deriving (Eq, Show)

data PicLevel = PicLevel String [PicBuffer] deriving (Eq, Show)

data PicBuffer = PicBuffer PicBufferId [PicElement] deriving (Eq, Show)

data PicElement = PicElement
  { src :: [(PicBufferId, Int)],
    value :: Int
  }
  deriving (Eq)

instance Show PicElement where
  show PicElement {src = [], value = v} = show v
  show PicElement {src = s, value = v} =
    show v ++ "^"
      ++ show
        ( map (\(PicBufferId d, _) -> d) s
        )

picBufferId :: PicBuffer -> PicBufferId
picBufferId (PicBuffer id _) = id

-- | Get a element of a pictbuffer by id
-- TODO: Make it return maybe
picBufferIndex :: PicBuffer -> Int -> Int
picBufferIndex (PicBuffer _ l) i = value (l !! i)

-- | Lookuptable of picts that can generate unique new names
data PicBufferLookup t = PicBufferLookup
  { lt :: [(t, PicBuffer)],
    cnt :: Int
  }
  deriving (Show)

-- | Empty PicBufferLookup without buffers
empty = PicBufferLookup {lt = [], cnt = 0}

-- | Rename the buffers (rename buffers with the actual buffers of a function)
renamePictBuffer :: (Show a, Eq a) => [(a, a)] -> PicBufferLookup a -> [(a, PicBuffer)]
renamePictBuffer lthaha PicBufferLookup {lt = oldLt} =
  map (first fromJust) $
    filter (\(gb, pb) -> isJust gb) $
      map (\(gb, pb) -> (lookup gb lthaha, pb)) oldLt

-- | Allocate a new pict buffer in the PicBufferLookup
addPictBuffer :: (Eq a, Show a) => a -> [PicElement] -> State (PicBufferLookup a) PicBuffer
addPictBuffer gpubuffer elemsnte = do
  PicBufferLookup {lt = l, cnt = num} <- get
  let nextNum = num + 1
  let b = PicBuffer (PicBufferId nextNum) elemsnte
  put $ PicBufferLookup {lt = (gpubuffer, b) : l, cnt = nextNum}
  return b

-- | Convert a Pic to a JSON
asJSON :: Pic -> BS.ByteString
asJSON = encode . toJSON

data NumberedElement = NumberedElement Int [(PicBufferId, Int)]

data NumberedBuffer = NumberedBuffer PicBufferId [NumberedElement]

data NumberedLevel = NumberedLevel String [NumberedBuffer]

instance ToJSON Pic where
  toJSON p = toJSON $ conv p

instance ToJSON NumberedElement where
  toJSON (NumberedElement v srcs) =
    object
      [ "value" .= toJSON v,
        "sources" .= toJSON srcs
      ]

instance ToJSON NumberedBuffer where
  toJSON (NumberedBuffer id els) =
    object
      [ "id" .= toJSON id,
        "contents" .= toJSON els
      ]

instance ToJSON NumberedLevel where
  toJSON (NumberedLevel id els) =
    object
      [ "comment" .= toJSON id,
        "buffers" .= toJSON els
      ]

instance ToJSON PicBufferId where
  toJSON (PicBufferId id) = toJSON id

conv :: Pic -> [NumberedLevel]
conv (Pic lvls) = map convLvl lvls

convLvl :: PicLevel -> NumberedLevel
convLvl (PicLevel name buffers) = NumberedLevel name $ map convBuffer buffers

convBuffer :: PicBuffer -> NumberedBuffer
convBuffer (PicBuffer id elements) = NumberedBuffer id $ map convElement elements

convElement :: PicElement -> NumberedElement
convElement PicElement {src = s, value = v} = NumberedElement v s
