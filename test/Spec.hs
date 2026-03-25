{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception    (SomeException, throwIO, try)
import           Data.Binary          (encode)
import           Data.Binary.Put      (putWord32be, runPut)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.IORef           (IORef, atomicModifyIORef', newIORef)
import           Data.List            (isInfixOf)
import           Data.Word            (Word32)
import           Hole.CRC16           (crc16)
import           Hole.Types           (Packet (..), PacketError (..), PacketType (..),
                                       maxDataLength)
import           Metro.Class          (RecvPacket (..))

main :: IO ()
main = do
  run "recvPacket rejects short header" testShortHeader
  run "recvPacket rejects too-small packet length" testSmallLength
  run "recvPacket rejects too-large packet length" testLargeLength
  run "recvPacket accepts valid packet" testValidPacket

run :: String -> IO () -> IO ()
run name action = do
  r <- try action :: IO (Either SomeException ())
  case r of
    Left e -> do
      putStrLn $ "FAIL: " ++ name
      throwIO e
    Right _ ->
      putStrLn $ "PASS: " ++ name

mkWirePacket :: PacketType -> ByteString -> ByteString
mkWirePacket pt body = LB.toStrict (encode pkt')
  where
    pkt0 = Packet 42 0 pt body
    pkt' = pkt0 {packetCrc = crc16 (LB.unpack (encode pkt0))}

mkHeader :: Word32 -> ByteString
mkHeader = LB.toStrict . runPut . putWord32be

mkRecv :: ByteString -> IO (Int -> IO ByteString)
mkRecv input = do
  ref <- newIORef input
  pure (takeBytes ref)
  where
    takeBytes :: IORef ByteString -> Int -> IO ByteString
    takeBytes ref n = atomicModifyIORef' ref $ \bs ->
      let (x, y) = B.splitAt n bs
      in (y, x)

expectDecodeError :: IO a -> (PacketError -> Bool) -> IO ()
expectDecodeError action predicate = do
  r <- try action
  case r of
    Left pe | predicate pe -> pure ()
    Left pe -> throwIO $ userError $ "unexpected PacketError: " ++ show pe
    Right _ -> throwIO $ userError "expected PacketError, but action succeeded"

testShortHeader :: IO ()
testShortHeader = do
  recv <- mkRecv "\x00\x00\x00"
  expectDecodeError ((recvPacket () (\_ -> pure ()) recv) :: IO Packet) $ \case
    PacketDecodeError _ -> True
    _                   -> False

testSmallLength :: IO ()
testSmallLength = do
  recv <- mkRecv (mkHeader 4)
  expectDecodeError ((recvPacket () (\_ -> pure ()) recv) :: IO Packet) $ \case
    PacketDecodeError msg -> "too small" `isInfixOf` msg
    _                     -> False

testLargeLength :: IO ()
testLargeLength = do
  recv <- mkRecv (mkHeader (fromIntegral (maxDataLength + 6)))
  expectDecodeError ((recvPacket () (\_ -> pure ()) recv) :: IO Packet) $ \case
    PacketDecodeError msg -> "too large" `isInfixOf` msg
    _                     -> False

testValidPacket :: IO ()
testValidPacket = do
  let body = "hello"
      wire = mkWirePacket Trns body
  recv <- mkRecv wire
  pkt <- recvPacket () (\_ -> pure ()) recv
  if packetType pkt == Trns && packetData pkt == body
    then pure ()
    else throwIO $ userError $ "unexpected packet: " ++ show pkt
