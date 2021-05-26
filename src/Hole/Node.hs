{-# LANGUAGE BangPatterns      #-}
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


import           Control.Monad       (forever, void)
import           Control.Monad.Cont  (callCC, runContT)
import           Control.Monad.Trans (lift)
import           Data.ByteString     (ByteString)
import           Data.Word           (Word16)
import           Hole.Types          (Packet (..), PacketType (..),
                                      getPacketData, maxDataLength, packet)
import           Metro.Class         (Transport (..))
import           Metro.Conn          (ConnEnv)
import           Metro.Node          (NodeEnv1, NodeT, SessionMode (..),
                                      initEnv1, request, runNodeT1,
                                      setSessionMode, startNodeT, stopNodeT)
import           Metro.Session       (SessionT, makeResponse_, receive, send)
import           System.Log.Logger   (errorM)
import           UnliftIO
import           UnliftIO.Concurrent (threadDelay)

type HoleT = NodeT () ByteString Word16 Packet

type HoleEnv = NodeEnv1 () ByteString Word16 Packet

type HoleSessionT = SessionT () ByteString Word16 Packet

initHoleEnv :: MonadIO m => ConnEnv tp -> ByteString -> m (HoleEnv tp)
initHoleEnv connEnv nid = do
  gen <- liftIO $ sessionGen (maxBound - 1000) maxBound
  initEnv1 (setSessionMode MultiAction) connEnv () nid True gen

sessionGen :: Word16 -> Word16 -> IO (IO Word16)
sessionGen start end = do
  gen <- newTVarIO start
  return $ atomically $ do
    v <- readTVar gen
    writeTVar gen $! (if v == end then start else v + 1)
    return v

pongHandler :: (MonadUnliftIO m, Transport tp) => HoleSessionT tp m ()
pongHandler = makeResponse_ $ \pkt ->
  case packetType pkt of
    Ping -> Just $ packet Pong ""
    Eof  -> Nothing
    Trns -> Nothing
    Pong -> Nothing

runHoleT :: Monad m => HoleEnv tp -> HoleT tp m a -> m a
runHoleT  = runNodeT1

checkAlive :: (MonadUnliftIO m, Transport tp) => HoleT tp m ()
checkAlive = void . async $ do
  (`runContT` pure) $ callCC $ \exit -> forever $ do
    threadDelay 10000000 -- 10s
    r <- lift . tryAny $ request (Just 10) $ packet Ping ""
    case r of
      Left e -> do
        liftIO $ errorM "Hole.Node" $ "CheckAlive Error: " ++ show e
        exit ()
      Right _ -> return ()

  stopNodeT

startHoleT :: (MonadUnliftIO m, Transport tp) => HoleEnv tp -> HoleSessionT tp m () -> m ()
startHoleT env sess = runHoleT env $ do
  checkAlive
  startNodeT sess

pipeHandler
  :: (MonadUnliftIO m, Transport tp, Transport tp1)
  => TransportConfig tp1 -> HoleSessionT tp m ()
pipeHandler config = do
  tp1 <- liftIO $ newTP config

  io1 <- async . (`runContT` pure) $ callCC $ \exit -> forever $ do
    !mpkt <- lift receive
    case mpkt of
      Nothing -> exit ()
      Just pkt     ->
        case packetType pkt of
          Ping    -> pure ()
          Eof     -> exit ()
          Pong    -> pure ()
          Trns    -> do
            r <- liftIO $ tryAny $ sendData tp1 $ getPacketData pkt
            case r of
              Left e -> do
                liftIO $ errorM "Hole.Node" $ "sendData Error: " ++ show e
                exit ()
              Right _ -> pure ()

  io0 <- async . (`runContT` pure) $ callCC $ \exit -> forever $ do
    !r <- liftIO $ tryAny $ recvData tp1 maxDataLength
    case r of
      Left e -> do
        liftIO $ errorM "Hole.Node" $ "recvData Error: " ++ show e
        exit ()
      Right "" -> exit ()
      Right bs -> lift $ send $ packet Trns bs

  void $ waitAnyCancel [io0, io1]
  send $ packet Eof ""
  liftIO $ closeTP tp1
