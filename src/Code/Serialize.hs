{-# LANGUAGE OverloadedStrings #-}

module Code.Serialize
  ( serialize,
    deserialize,
  )
where

import Code.Definitions
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.List
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Text
import qualified Data.Maybe

-- | Convert device and hostcode to a bytestring
serialize :: String -> [GPUAction] -> BS.ByteString
serialize deviceCode hostCode =
  BS.concat
    [ BSC.pack "Gaiwan",
      BSC.pack (show $ BS.length serializedHostCode),
      BSC.pack "\n",
      serializedHostCode,
      BSC.pack "\n",
      BSC.pack deviceCode
    ]
  where
    serializedHostCode = encode hostCode

posOfFirsNewline = BSS.elemIndex 10

-- | Convert a bytestring to device and hostcode
deserialize :: BS.ByteString -> Maybe (String, [GPUAction])
deserialize e = do
  cutIndex <- posOfFirsNewline ee
  let (preGaiwan, postGaiwan) = BSS.splitAt (cutIndex + 1) ee
  cutIndex2 <- posOfFirsNewline postGaiwan
  let (codePart, deviceCodePart) = BSS.splitAt cutIndex2 postGaiwan
  dd <- decodeHost codePart
  return (BSSC.unpack deviceCodePart, dd)
  where
    ee = BS.toStrict e
    decodeHost :: BSS.ByteString -> Maybe [GPUAction]
    decodeHost z = fixBuffers <$> decodeStrict' z
    fixBuffers :: [GPUAction] -> [GPUAction]
    fixBuffers i = Prelude.map ab i
      where
        ab :: GPUAction -> GPUAction
        ab (CallKernel name used out threads) = CallKernel name (map c used) (map c used) threads
        ab (ReadBuffer b) = ReadBuffer (c b)
        ab  b = b
        lt :: [(GPUBufferName, GPUBuffer)]
        lt = map tt (filter isAlloc i)
        tt (AllocBuffer b@(GPUBuffer n@(GPUBufferName _) _)) = (n,b)
        isAlloc (AllocBuffer _) = True
        isAlloc _ = False
        c :: GPUBuffer -> GPUBuffer
        c (GPUBuffer n 0) = Data.Maybe.fromJust $ lookup n lt

-- JSON definition below

instance ToJSON GPUAction where
  toJSON (ReadBuffer b) = object ["read" .= b]
  toJSON (CallKernel name bufs outbufs threads) =
    object
      [ "call" .= name,
        "buffers" .=  [ bufs, outbufs ],
        "threads" .= threads
      ]
  toJSON (AllocBuffer (GPUBuffer (GPUBufferName number) size)) =
    object
      [ "alloc" .= number,
        "size" .= size
      ]

instance FromJSON GPUAction where
  parseJSON (Object v) =
    CallKernel
      <$> v .: "call"
      <*> ( (\[a,_] -> a) <$> (v .: "buffers"))
      <*> ( (\[_,a] -> a) <$> (v .: "buffers"))
      <*> (v .: "threads")
      <|> ReadBuffer <$> v .: "read"
      <|> (\n s -> AllocBuffer $ GPUBuffer (GPUBufferName n) s) <$> v .: "alloc" <*> v.:"size"
  parseJSON _ = mzero

instance ToJSON GPUBuffer where
  toJSON (GPUBuffer (GPUBufferName number) size) = toJSON number

instance FromJSON GPUBuffer where
  parseJSON v@(Number _) = do
    number <- parseJSON v
    return $ GPUBuffer (GPUBufferName number) 0 -- hack!!!
  parseJSON _ = mzero

instance ToJSON KernelName where
  toJSON (KernelName s) = toJSON s

instance FromJSON KernelName where
  parseJSON (String v) = return $ KernelName $ Data.Text.unpack v
  parseJSON _ = mzero
