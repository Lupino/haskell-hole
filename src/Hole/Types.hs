{-# LANGUAGE MultiParamTypeClasses #-}

module Hole.Types
  ( Packet (..)
  , packet
  , getPacketData
  , maxDataLength
  , formatMessage
  ) where

import           Control.Exception     (Exception)
import           Data.Binary           (Binary (..), decode, decodeOrFail,
                                        encode)
import           Data.Binary.Get       (getByteString, getWord16be, getWord32be)
import           Data.Binary.Put       (putByteString, putWord16be, putWord32be)
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B (length)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import           Data.ByteString.Lazy  (fromStrict, toStrict)
import qualified Data.ByteString.Lazy  as LB (unpack)
import           Data.Word             (Word16)
import           Hole.CRC16            (crc16)
import           Metro.Class           (GetPacketId (..), RecvPacket (..),
                                        SendPacket (..), SetPacketId (..),
                                        sendBinary)
import           UnliftIO              (throwIO)

maxDataLength :: Int
maxDataLength = 41943040 -- 40m


newtype PacketLength = PacketLength Int
  deriving (Show, Eq)

instance Binary PacketLength where
  get = PacketLength . fromIntegral <$> getWord32be
  put (PacketLength l) = putWord32be $ fromIntegral l


data Packet = Packet
  { packetId   :: !Word16
  , packetCrc  :: !Word16
  , packetData :: !ByteString
  }
  deriving (Show, Eq)

instance Binary Packet where
  get = do
    PacketLength len <- get
    pid <- getWord16be
    crc <- getWord16be
    Packet pid crc <$> getByteString (len - 4)
  put (Packet pid crc body) = do
    put $ PacketLength $ B.length body + 4
    putWord16be pid
    putWord16be crc
    putByteString body

preparePacket :: Packet -> Packet
preparePacket pkt = pkt { packetCrc = calcCrc16 pkt }

calcCrc16 :: Packet -> Word16
calcCrc16 pkt = crc16 . LB.unpack $ encode pkt'
  where pkt' = pkt { packetCrc = 0 }

instance RecvPacket Packet where
  recvPacket recv = do
    hbs <- recv 4
    case decode (fromStrict hbs) of
      PacketLength len -> do
        bs <- recv len
        case decodeOrFail (fromStrict $ hbs <> bs) of
          Left (_, _, e1)   -> throwIO $ PacketDecodeError $ "Packet: " <> e1
          Right (_, _, pkt) -> do
            if packetCrc pkt == calcCrc16 pkt then return pkt
                                              else throwIO PacketCrcNotMatch

instance SendPacket Packet where
  sendPacket = sendBinary . preparePacket

instance GetPacketId Word16 Packet where
  getPacketId = packetId

instance SetPacketId Word16 Packet where
  setPacketId k pkt = pkt { packetId = k }

packet :: ByteString -> Packet
packet = Packet 0 0

getPacketData :: Packet -> ByteString
getPacketData = packetData

data PacketError = PacketDecodeError String | PacketCrcNotMatch
  deriving (Show, Eq, Ord)

instance Exception PacketError

formatMessage :: String -> String
formatMessage = B.unpack . toStrict . encode . preparePacket . packet . B.pack
