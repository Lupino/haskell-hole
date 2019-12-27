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

import           Control.Monad             (forever, mzero, void)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (runMaybeT)
import           Data.ByteString           (ByteString)
import           Data.Word                 (Word16)
import           Hole.Types                (Packet, getPacketData, packet)
import           Metro.Class               (Transport (..))
import           Metro.Conn                (ConnEnv)
import           Metro.Node                (NodeEnv1, NodeT, SessionMode (..),
                                            initEnv1, request, runNodeT1,
                                            setSessionMode, startNodeT,
                                            stopNodeT)
import           Metro.Session             (SessionT, makeResponse_, receive,
                                            send)
import           System.Log.Logger         (errorM)
import           UnliftIO
import           UnliftIO.Concurrent       (threadDelay)

type HoleT = NodeT () ByteString Word16 Packet

type HoleEnv = NodeEnv1 () ByteString Word16 Packet

type HoleSessionT = SessionT () ByteString Word16 Packet

initHoleEnv :: MonadIO m => ConnEnv tp -> ByteString -> m (HoleEnv tp)
initHoleEnv connEnv nid = do
  gen <- liftIO $ sessionGen (maxBound - 1000) maxBound
  initEnv1 (setSessionMode MultiAction) connEnv () nid gen

sessionGen :: Word16 -> Word16 -> IO (IO Word16)
sessionGen start end = do
  gen <- newTVarIO start
  return $ atomically $ do
    v <- readTVar gen
    writeTVar gen $! (if v == end then start else v + 1)
    return v

pongHandler :: (MonadUnliftIO m, Transport tp) => HoleSessionT tp m ()
pongHandler = makeResponse_ $ \pkt ->
  case getPacketData pkt of
    ""    -> Nothing
    "EOF" -> Nothing
    _     -> Just $ packet "EOF"

runHoleT :: Monad m => HoleEnv tp -> HoleT tp m a -> m a
runHoleT  = runNodeT1

checkAlive :: (MonadUnliftIO m, Transport tp) => HoleT tp m ()
checkAlive = void . async $ do
  void . runMaybeT . forever $ do
    threadDelay 10000000 -- 10s
    r <- lift . tryAny $ request (Just 10) $ packet "PING"
    case r of
      Left e -> do
        liftIO $ errorM "Hole.Node" $ "CheckAlive Error: " ++ show e
        mzero
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
  tp1 <- liftIO $ newTransport config

  io1 <- async . void . runMaybeT . forever $ do
    !body <- lift $ fmap getPacketData <$> receive
    case body of
      Nothing    -> mzero
      Just ""    -> mzero
      Just "EOF" -> mzero
      Just bs    -> do
        r <- liftIO $ tryAny $ sendData tp1 bs
        case r of
          Left e -> do
            liftIO $ errorM "Hole.Node" $ "sendData Error: " ++ show e
            mzero
          Right _ -> pure ()

  io0 <- async . void . runMaybeT . forever $ do
    !r <- liftIO $ tryAny $ recvData tp1 41943040 -- 40M
    case r of
      Left e -> do
        liftIO $ errorM "Hole.Node" $ "recvData Error: " ++ show e
        mzero
      Right ""     -> mzero
      Right "EOF"  -> mzero
      Right bs     -> lift $ send $ packet bs

  void $ waitAnyCancel [io0, io1]
  send $ packet "EOF"
  liftIO $ closeTransport tp1
