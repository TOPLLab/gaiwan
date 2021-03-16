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
import qualified Data.ByteString.Lazy as BS
import Data.Text

-- | Convert device and hostcode to a bytestring
serialize :: String -> [GPUAction] -> BS.ByteString
serialize deviceCode hostCode = encode (String "Gaiwan", deviceCode, hostCode)

-- | Convert a bytestring to device and hostcode
deserialize :: BS.ByteString -> Maybe (String, [GPUAction])
deserialize e = decodeTriple e >>= checkMagic
  where
    decodeTriple :: BS.ByteString -> Maybe (String, String, [GPUAction])
    decodeTriple = decode
    checkMagic (magic, deviceCode, hostCode) | magic == "Gaiwan" = Just (deviceCode, hostCode)
    checkMagic _ = Nothing

-- JSON definition below

instance ToJSON GPUAction where
  toJSON (ReadBuffer b) = object ["read" .= b]
  toJSON (CallKernel name bufs outbufs threads) =
    object
      [ "call" .= name,
        "buffers"
          .= object
            [ "used" .= bufs,
              "out" .= outbufs
            ],
        "threads" .= threads
      ]
  toJSON (AllocBuffer buffer) = object ["alloc" .= buffer]

instance FromJSON GPUAction where
  parseJSON (Object v) =
    (ReadBuffer <$> v .: "read")
      <|> ( CallKernel
              <$> v .: "call"
              <*> ((v .: "buffers") >>= (.: "used"))
              <*> ((v .: "buffers") >>= (.: "out"))
              <*> (v .: "threads")
          )
      <|> (AllocBuffer <$> v .: "alloc")
  parseJSON _ = mzero

instance ToJSON GPUBuffer where
  toJSON (GPUBuffer (GPUBufferName number) size) = toJSON [number, size]

instance FromJSON GPUBuffer where
  parseJSON v@(Array _) = do
    [number, size] <- parseJSON v
    return $ GPUBuffer (GPUBufferName number) size
  parseJSON _ = mzero

instance ToJSON KernelName where
  toJSON (KernelName s) = toJSON s

instance FromJSON KernelName where
  parseJSON (String v) = return $ KernelName $ unpack v
  parseJSON _ = mzero
