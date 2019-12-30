{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Hole
  ( Config (..)
  , ProxyMode (..)
  , startHoleServer
  , startHoleClient
  ) where

import           Data.ByteString        (ByteString)
import           Data.List              (sortOn)
import           Data.Word              (Word16)
import           Hole.Node              (HoleEnv, HoleSessionT, initHoleEnv,
                                         pipeHandler, pongHandler, runHoleT,
                                         sessionGen, startHoleT)
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

data ProxyMode = LocalToRemote | RemoteToLocal
  deriving (Show, Eq)

data Config = Config
  { holeSockPort :: String
  , outSockPort  :: String
  , logLevel     :: Priority
  , proxyMode    :: ProxyMode
  }

type HoleServerEnv serv = ServerEnv serv () ByteString Word16 Packet
type HoleNodeList tp = IOHashMap ByteString (HoleEnv tp)

newHoleServer
  :: (Servable serv, Transport tp)
  => (TransportConfig (STP serv) -> TransportConfig tp)
  -> ServerConfig serv
  -> HoleSessionT tp IO ()
  -> IO (Async (), HoleServerEnv serv tp)
newHoleServer mk config sess = do
  gen <- sessionGen 1 (maxBound - 1000)
  sEnv <- fmap mapEnv . initServerEnv config gen mk $ \_ connEnv -> do
    dat <- getPacketData <$> runConnT connEnv receive
    return $ Just (dat, ())
  io <- async $ startServer sEnv sess
  return (io, sEnv)

  where mapEnv :: HoleServerEnv serv tp -> HoleServerEnv serv tp
        mapEnv = setServerName "Hole" . S.setSessionMode MultiAction

doAction
  :: (MonadUnliftIO m, Transport tp0, Transport tp1)
  => HoleNodeList tp0 -> TransportConfig tp1 -> m ()
doAction nodeList config = do
  nl <- mapM mapFunc =<< HM.elems nodeList
  case sortOn fst nl of
    [] -> liftIO $ errorM "Hole" "Client not found."
    ((_, nodeEnv):_) ->
      runHoleT nodeEnv $ withSessionT Nothing $ pipeHandler config

  where mapFunc x = do
          size <- getSessionSize1 x
          return (size, x)

startHoleOutServer
  :: (Servable serv1, Transport (STP serv1), Transport tp1)
  => (TransportConfig tp1 -> IO ()) -> ServerConfig serv1 -> (TransportConfig (STP serv1) -> TransportConfig tp1) -> IO ()
startHoleOutServer action config mapTrans = do
  outSEnv <- initOutServerEnv config
  startOutServer outSEnv $ action . mapTrans

startHoleServerLR_
  :: (Servable serv0, Servable serv1, Transport (STP serv0), Transport (STP serv1), Transport tp0, Transport tp1)
  => ServerConfig serv0
  -> ServerConfig serv1
  -> (TransportConfig (STP serv0) -> TransportConfig tp0)
  -> (TransportConfig (STP serv1) -> TransportConfig tp1)
  -> IO ()
startHoleServerLR_ config0 config1 mapT0 mapT1 = do
  (_, sEnv) <- newHoleServer mapT0 config0 pongHandler
  startHoleOutServer (doAction (getNodeEnvList sEnv)) config1 mapT1

startHoleServerRL_
  :: (Servable serv0, Transport (STP serv0), Transport tp0, Transport tp1)
  => ServerConfig serv0
  -> TransportConfig tp1
  -> (TransportConfig (STP serv0) -> TransportConfig tp0)
  -> IO ()
startHoleServerRL_ config0 config1 mapT0 = do
  (io, _) <- newHoleServer mapT0 config0 (pipeHandler config1)
  r <- waitCatch io
  case r of
    Left e  -> liftIO $ errorM "Hole" $ "HoleServer error " ++ show e
    Right _ -> return ()

startHoleServerLR :: Config -> IO ()
startHoleServerLR Config {..} = do
  setupLog logLevel
  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleServer" Raw
        m1 = debugConfig "OutServer" Raw

    startHoleServerLR_ (socketServer holeSockPort) (socketServer outSockPort) m0 m1
  else
    startHoleServerLR_ (socketServer holeSockPort) (socketServer outSockPort) id id

startHoleServerRL :: Config -> IO ()
startHoleServerRL Config {..} = do
  setupLog logLevel
  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleServer" Raw
        m1 = debugConfig "OutClient" Raw

    startHoleServerRL_ (socketServer holeSockPort) (m1 $ socket outSockPort) m0
  else
    startHoleServerRL_ (socketServer holeSockPort) (socket outSockPort) id

startHoleServer :: Config -> IO ()
startHoleServer config =
  case proxyMode config of
    LocalToRemote -> startHoleServerLR config
    RemoteToLocal -> startHoleServerRL config

startHoleClient :: ByteString -> Config -> IO ()
startHoleClient cid config =
  case proxyMode config of
    LocalToRemote -> startHoleClientLR cid config
    RemoteToLocal -> startHoleClientRL cid config

startHoleClientLR :: ByteString -> Config -> IO ()
startHoleClientLR cid Config {..} = do
  setupLog logLevel
  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleClient" Raw
        m1 = debugConfig "OutClient" Raw
    runHoleClientLR cid (m0 $ socket holeSockPort) (m1 $ socket outSockPort)
  else
    runHoleClientLR cid (socket holeSockPort) (socket outSockPort)

startHoleClientRL :: ByteString -> Config -> IO ()
startHoleClientRL cid Config {..} = do
  setupLog logLevel
  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleClient" Raw
        m1 = debugConfig "OutServer" Raw
    runHoleClientRL cid (m0 $ socket holeSockPort) (socketServer outSockPort) m1
  else
    runHoleClientRL cid (socket holeSockPort) (socketServer outSockPort) id

runHoleClientLR
  :: (Transport tp0, Transport tp1)
  => ByteString
  -> TransportConfig tp0
  -> TransportConfig tp1
  -> IO ()
runHoleClientLR cid config0 config1 = do
  (io, _) <- newHoleClient cid config0 $ pipeHandler config1
  r <- waitCatch io
  case r of
    Left e  -> liftIO $ errorM "Hole" $ "HoleClient error " ++ show e
    Right _ -> return ()

runHoleClientRL
  :: (Servable serv1, Transport (STP serv1), Transport tp0, Transport tp1)
  => ByteString
  -> TransportConfig tp0
  -> ServerConfig serv1
  -> (TransportConfig (STP serv1) -> TransportConfig tp1)
  -> IO ()
runHoleClientRL cid config0 config1 mapT1 = do
  (_, nodeEnv) <- newHoleClient cid config0 pongHandler
  startHoleOutServer (runHoleT nodeEnv . withSessionT Nothing . pipeHandler) config1 mapT1

newHoleClient
  :: (Transport tp)
  => ByteString
  -> TransportConfig tp
  -> HoleSessionT tp IO ()
  -> IO (Async (), HoleEnv tp)
newHoleClient cid config sess = do
  connEnv <- initConnEnv config
  runConnT connEnv $ send $ packet cid
  env0 <- initHoleEnv connEnv cid
  io <- async $ startHoleT env0 sess
  return (io, env0)
