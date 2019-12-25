{-# LANGUAGE MultiParamTypeClasses #-}

module Hole.Types
  ( Packet (..)
  , packet
  , getPacketData
  ) where

import           Data.Binary          (Binary (..), decode)
import           Data.Binary.Get      (getByteString, getWord16be)
import           Data.Binary.Put      (putByteString, putWord16be)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B (length)
import           Data.ByteString.Lazy (fromStrict)
import           Data.Word            (Word16)
import           Metro.Class          (GetPacketId (..), RecvPacket (..),
                                       SendPacket (..), SetPacketId (..))


newtype PacketLength = PacketLength Int
  deriving (Show, Eq)

instance Binary PacketLength where
  get = PacketLength . fromIntegral <$> getWord16be
  put (PacketLength l) = putWord16be $ fromIntegral l


data Packet = Packet
  { packetId   :: !Word16
  , packetData :: !ByteString
  }
  deriving (Show, Eq)

instance Binary Packet where
  get = do
    PacketLength len <- get
    pid <- getWord16be
    Packet pid <$> getByteString (len - 2)
  put (Packet pid body) = do
    put $ PacketLength $ B.length body + 2
    putWord16be pid
    putByteString body

instance RecvPacket Packet where
  recvPacket recv = do
    hbs <- recv 2
    case decode (fromStrict hbs) of
      PacketLength len -> do
        bs <- recv len
        return $ decode . fromStrict $ hbs <> bs

instance SendPacket Packet

instance GetPacketId Word16 Packet where
  getPacketId = packetId

instance SetPacketId Word16 Packet where
  setPacketId k pkt = pkt { packetId = k }

packet :: ByteString -> Packet
packet = Packet 0

getPacketData :: Packet -> ByteString
getPacketData = packetData
