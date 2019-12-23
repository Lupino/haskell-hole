{-# LANGUAGE OverloadedStrings #-}

module Hole.Node
  ( HoleT
  , HoleEnv
  , HoleSessionT
  , initHoleEnv
  , runHoleT
  , startHoleT

  , sessionGen
  , pongHandler
  , pipeHandler
  ) where

import           Data.ByteString (ByteString)
import           Data.Word       (Word16)
import           Hole.Types      (Packet, getPacketData, packet)
import           Metro.Class     (Transport (..))
import           Metro.Conn      (ConnEnv)
import           Metro.Node      (NodeEnv1, NodeT, SessionMode (..), initEnv1,
                                  runNodeT1, setSessionMode, startNodeT)
import           Metro.Session   (SessionT, makeResponse_, receive, send)
import           UnliftIO

type HoleT = NodeT () ByteString Word16 Packet

type HoleEnv = NodeEnv1 () ByteString Word16 Packet

type HoleSessionT = SessionT () ByteString Word16 Packet

initHoleEnv :: MonadIO m => ConnEnv tp -> ByteString -> m (HoleEnv tp)
initHoleEnv connEnv nid = do
  gen <- liftIO sessionGen
  initEnv1 (setSessionMode MultiAction) connEnv () nid gen

sessionGen :: IO (IO Word16)
sessionGen = do
  gen <- newTVarIO 1
  return $ atomically $ do
    v <- readTVar gen
    writeTVar gen $! (if v == maxBound then 1 else v + 1)
    return v

pongHandler :: (MonadUnliftIO m, Transport tp) => HoleSessionT tp m ()
pongHandler = makeResponse_ . const . Just $ packet "pong"

runHoleT :: Monad m => HoleEnv tp -> HoleT tp m a -> m a
runHoleT  = runNodeT1

startHoleT :: (MonadUnliftIO m, Transport tp) => HoleEnv tp -> HoleSessionT tp m () -> m ()
startHoleT env sess = runHoleT env $ startNodeT sess

pipeHandler
  :: (MonadUnliftIO m, Transport tp, Transport tp1)
  => TransportConfig tp1 -> HoleSessionT tp m ()
pipeHandler config = do
  tp1 <- liftIO $ newTransport config
  io <- async $ do
    bs <- liftIO $ recvData tp1 1024
    send $ packet bs

  body <- fmap getPacketData <$> receive
  case body of
    Nothing -> do
      liftIO $ closeTransport tp1
      cancel io
    Just bs -> liftIO $ sendData tp1 bs
