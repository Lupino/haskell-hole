{-# LANGUAGE MultiParamTypeClasses #-}

module Hole.Types
  ( Packet (..)
  , PacketType (..)
  , packet
  , getPacketData
  , maxDataLength
  ) where

import           Control.Exception    (Exception)
import           Data.Binary          (Binary (..), decode, decodeOrFail,
                                       encode, getWord8, putWord8)
import           Data.Binary.Get      (getByteString, getWord16be, getWord32be)
import           Data.Binary.Put      (putByteString, putWord16be, putWord32be)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B (length)
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as LB (unpack)
import           Data.Word            (Word16)
import           Hole.CRC16           (crc16)
import           Metro.Class          (GetPacketId (..), RecvPacket (..),
                                       SendPacket (..), SetPacketId (..),
                                       sendBinary)
import           UnliftIO             (throwIO)

maxDataLength :: Int
maxDataLength = 41943040 -- 40m


newtype PacketLength = PacketLength Int
  deriving (Show, Eq)

instance Binary PacketLength where
  get = PacketLength . fromIntegral <$> getWord32be
  put (PacketLength l) = putWord32be $ fromIntegral l


data PacketType = Ping | Trns | Eof | Pong
  deriving (Show, Eq)


instance Binary PacketType where
  get = do
    v <- getWord8
    case v of
      0 -> pure Ping
      1 -> pure Trns
      2 -> pure Eof
      3 -> pure Pong
      _ -> fail $ "not such type " ++ show v
  put Ping = putWord8 0
  put Trns = putWord8 1
  put Eof  = putWord8 2
  put Pong = putWord8 3


data Packet = Packet
  { packetId   :: !Word16
  , packetCrc  :: !Word16
  , packetType :: !PacketType
  , packetData :: !ByteString
  }
  deriving (Show, Eq)

instance Binary Packet where
  get = do
    PacketLength len <- get
    pid <- getWord16be
    crc <- getWord16be
    ptp <- get
    Packet pid crc ptp <$> getByteString (len - 5)
  put (Packet pid crc ptp body) = do
    put $ PacketLength $ B.length body + 5
    putWord16be pid
    putWord16be crc
    put ptp
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

packet :: PacketType -> ByteString -> Packet
packet = Packet 0 0

getPacketData :: Packet -> ByteString
getPacketData = packetData

data PacketError = PacketDecodeError String | PacketCrcNotMatch
  deriving (Show, Eq, Ord)

instance Exception PacketError
