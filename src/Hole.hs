{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Hole
  ( Config (..)
  , startHoleServer
  , startHoleClient
  ) where

import           Control.Monad             (void)
import           Data.ByteString           (ByteString)
import           Data.List                 (isPrefixOf, (!!))
import           Data.Word                 (Word16)
import           Hole.Node                 (HoleEnv, initHoleEnv, pipeHandler,
                                            pongHandler, runHoleT, sessionGen,
                                            startHoleT)
import           Hole.OutServer
import           Hole.Types
import           Metro.Class               (Servable (STP, ServerConfig),
                                            Transport (TransportConfig))
import           Metro.Conn                (initConnEnv, receive, runConnT,
                                            send)
import           Metro.IOHashMap           (IOHashMap)
import qualified Metro.IOHashMap           as HM (elems)
import           Metro.Node                (SessionMode (..), withSessionT)
import           Metro.Server              (ServerEnv, getNodeEnvList,
                                            initServerEnv, setServerName,
                                            startServer)
import qualified Metro.Server              as S (setSessionMode)
import           Metro.TCP                 (tcpConfig)
import           Metro.Transport.Debug     (DebugMode (..), debugConfig)
import           Metro.Transport.Socket    (socketUri)
import           Metro.Transport.UDPSocket (udpSocket)
import           Metro.UDP                 (udpConfig)
import           Metro.Utils               (setupLog)
import           System.Log                (Priority (..))
import           System.Log.Logger         (errorM)
import           System.Random             (randomRIO)
import           UnliftIO

data Config = Config
  { holeSockPort :: String
  , outSockPort  :: String
  , logLevel     :: Priority
  }

type HoleServerEnv serv = ServerEnv serv () ByteString Word16 Packet
type HoleNodeList tp = IOHashMap ByteString (HoleEnv tp)

newHoleServer
  :: (Servable serv, Transport tp)
  => (TransportConfig (STP serv) -> TransportConfig tp)
  -> ServerConfig serv
  -> (HoleServerEnv serv tp -> HoleServerEnv serv tp)
  -> IO (HoleServerEnv serv tp)
newHoleServer mk config mapEnv = do
  gen <- sessionGen
  sEnv <- fmap mapEnv . initServerEnv config gen mk $ \_ connEnv -> do
    dat <- getPacketData <$> runConnT connEnv receive
    return $ Just (dat, ())
  void $ async $ startServer sEnv pongHandler
  return sEnv

action
  :: (MonadUnliftIO m, Transport tp0, Transport tp1)
  => HoleNodeList tp0 -> TransportConfig tp1 -> m ()
action nodeList config = do
  nl <- HM.elems nodeList
  case nl of
    [] -> liftIO $ errorM "Hole" "Client not found."
    [nodeEnv] -> runHoleT nodeEnv $ withSessionT Nothing $ pipeHandler config
    _ -> do
      idx <- liftIO $ randomRIO (0, length nl - 1)
      let nodeEnv = nl !! idx
      runHoleT nodeEnv $ withSessionT Nothing $ pipeHandler config

startHoleOutServer
  :: (Servable serv1, Transport tp0, Transport (STP serv1), Transport tp1)
  => HoleNodeList tp0 -> ServerConfig serv1 -> (TransportConfig (STP serv1) -> TransportConfig tp1) -> IO ()
startHoleOutServer nodeList config mapTrans = do
  outSEnv <- initOutServerEnv config
  startOutServer outSEnv $ action nodeList . mapTrans

startHoleServer_
  :: (Servable serv0, Servable serv1, Transport (STP serv0), Transport (STP serv1), Transport tp0, Transport tp1)
  => ServerConfig serv0
  -> ServerConfig serv1
  -> (TransportConfig (STP serv0) -> TransportConfig tp0)
  -> (TransportConfig (STP serv1) -> TransportConfig tp1)
  -> IO ()
startHoleServer_ config0 config1 mapT0 mapT1 = do
  sEnv <- newHoleServer mapT0 config0 mapEnv
  startHoleOutServer (getNodeEnvList sEnv) config1 mapT1

  where mapEnv :: HoleServerEnv serv tp -> HoleServerEnv serv tp
        mapEnv = setServerName "Hole" . S.setSessionMode MultiAction

startHoleServer :: Config -> IO ()
startHoleServer Config {..} = do
  setupLog logLevel
  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleServer" Raw
        m1 = debugConfig "OutServer" Raw
    if "udp" `isPrefixOf` holeSockPort then
      if "udp" `isPrefixOf` outSockPort then
        startHoleServer_ (udpConfig holeSockPort) (udpConfig outSockPort) m0 m1
      else
        startHoleServer_ (udpConfig holeSockPort) (tcpConfig outSockPort) m0 m1
    else
      if "udp" `isPrefixOf` outSockPort then
        startHoleServer_ (tcpConfig holeSockPort) (udpConfig outSockPort) m0 m1
      else
        startHoleServer_ (tcpConfig holeSockPort) (tcpConfig outSockPort) m0 m1
  else
    if "udp" `isPrefixOf` holeSockPort then
      if "udp" `isPrefixOf` outSockPort then
        startHoleServer_ (udpConfig holeSockPort) (udpConfig outSockPort) id id
      else
        startHoleServer_ (udpConfig holeSockPort) (tcpConfig outSockPort) id id
    else
      if "udp" `isPrefixOf` outSockPort then
        startHoleServer_ (tcpConfig holeSockPort) (udpConfig outSockPort) id id
      else
        startHoleServer_ (tcpConfig holeSockPort) (tcpConfig outSockPort) id id

startHoleClient :: ByteString -> Config -> IO ()
startHoleClient cid Config {..} = do
  setupLog logLevel
  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleClient" Raw
        m1 = debugConfig "OutClient" Raw
    if "udp" `isPrefixOf` holeSockPort then
      if "udp" `isPrefixOf` outSockPort then
        runHoleClient cid (m0 $ udpSocket holeSockPort) (m1 $ udpSocket outSockPort)
      else
        runHoleClient cid (m0 $ udpSocket holeSockPort) (m1 $ socketUri outSockPort)
    else
      if "udp" `isPrefixOf` outSockPort then
        runHoleClient cid (m0 $ socketUri holeSockPort) (m1 $ udpSocket outSockPort)
      else
        runHoleClient cid (m0 $ socketUri holeSockPort) (m1 $ socketUri outSockPort)
  else
    if "udp" `isPrefixOf` holeSockPort then
      if "udp" `isPrefixOf` outSockPort then
        runHoleClient cid (udpSocket holeSockPort) (udpSocket outSockPort)
      else
        runHoleClient cid (udpSocket holeSockPort) (socketUri outSockPort)
    else
      if "udp" `isPrefixOf` outSockPort then
        runHoleClient cid (socketUri holeSockPort) (udpSocket outSockPort)
      else
        runHoleClient cid (socketUri holeSockPort) (socketUri outSockPort)

runHoleClient
  :: (Transport tp0, Transport tp1)
  => ByteString
  -> TransportConfig tp0
  -> TransportConfig tp1
  -> IO ()
runHoleClient cid config0 config1 = do
    connEnv <- initConnEnv config0
    runConnT connEnv $ send $ packet cid
    env0 <- initHoleEnv connEnv cid
    startHoleT env0 $ pipeHandler config1
