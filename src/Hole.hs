{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Hole
  ( Config (..)
  , startHoleServer
  , startHoleClient
  ) where

import           Control.Monad          (void)
import           Data.ByteString        (ByteString)
import           Data.List              (sortOn)
import           Data.Word              (Word16)
import           Hole.Node              (HoleEnv, initHoleEnv, pipeHandler,
                                         pongHandler, runHoleT, sessionGen,
                                         startHoleT)
import           Hole.OutServer
import           Hole.Types
import           Metro.Class            (Servable (STP, ServerConfig),
                                         Transport (TransportConfig))
import           Metro.Conn             (initConnEnv, receive, runConnT, send)
import           Metro.IOHashMap        (IOHashMap)
import qualified Metro.IOHashMap        as HM (elems)
import           Metro.Node             (SessionMode (..), getSessionSize1,
                                         withSessionT)
import           Metro.Server           (ServerEnv, getNodeEnvList,
                                         initServerEnv, setServerName,
                                         startServer)
import qualified Metro.Server           as S (setSessionMode)
import           Metro.SocketServer     (socketServer)
import           Metro.Transport.Debug  (DebugMode (..), debugConfig)
import           Metro.Transport.Socket (socket)
import           Metro.Utils            (setupLog)
import           System.Log             (Priority (..))
import           System.Log.Logger      (errorM)
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
  gen <- sessionGen 1 (maxBound - 1000)
  sEnv <- fmap mapEnv . initServerEnv config gen mk $ \_ connEnv -> do
    dat <- getPacketData <$> runConnT connEnv receive
    return $ Just (dat, ())
  void $ async $ startServer sEnv pongHandler
  return sEnv

action
  :: (MonadUnliftIO m, Transport tp0, Transport tp1)
  => HoleNodeList tp0 -> TransportConfig tp1 -> m ()
action nodeList config = do
  nl <- mapM mapFunc =<< HM.elems nodeList
  case sortOn fst nl of
    [] -> liftIO $ errorM "Hole" "Client not found."
    ((_, nodeEnv):_) ->
      runHoleT nodeEnv $ withSessionT Nothing $ pipeHandler config

  where mapFunc x = do
          size <- getSessionSize1 x
          return (size, x)

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

    startHoleServer_ (socketServer holeSockPort) (socketServer outSockPort) m0 m1
  else
    startHoleServer_ (socketServer holeSockPort) (socketServer outSockPort) id id

startHoleClient :: ByteString -> Config -> IO ()
startHoleClient cid Config {..} = do
  setupLog logLevel
  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleClient" Raw
        m1 = debugConfig "OutClient" Raw
    runHoleClient cid (m0 $ socket holeSockPort) (m1 $ socket outSockPort)
  else
    runHoleClient cid (socket holeSockPort) (socket outSockPort)

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
