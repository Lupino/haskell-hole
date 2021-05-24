{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hole
  ( Config (..)
  , ProxyMode (..)
  , startHoleServer
  , startHoleClient
  ) where


import           Crypto.Cipher.AES
import           Crypto.Cipher.Blowfish
import           Crypto.Cipher.CAST5
import           Crypto.Cipher.Camellia
import           Crypto.Cipher.DES
import           Crypto.Cipher.TripleDES
import           Crypto.Cipher.Twofish
import           Crypto.Cipher.Types     (BlockCipher (..), Cipher (..))
import           Data.ByteString         (ByteString)
import           Data.IOMap              (IOMap)
import qualified Data.IOMap              as Map (elems)
import           Data.List               (sortOn)
import           Data.Word               (Word16)
import           Hole.Node               (HoleEnv, HoleSessionT, initHoleEnv,
                                          pipeHandler, pongHandler, runHoleT,
                                          sessionGen, startHoleT)
import           Hole.OutServer
import           Hole.Types
import           Metro.Class             (Servable (STP, ServerConfig),
                                          Transport (TransportConfig))
import           Metro.Conn              (initConnEnv, receive, runConnT, send)
import           Metro.Node              (SessionMode (..), getSessionSize1,
                                          withSessionT)
import           Metro.Server            (ServerEnv, getNodeEnvList,
                                          initServerEnv, setServerName,
                                          startServer)
import qualified Metro.Server            as S (setSessionMode)
import           Metro.SocketServer      (socketServer)
import           Metro.TP.Crypto         (makeCrypto)
import           Metro.TP.Debug          (DebugMode (..), debugConfig)
import           Metro.TP.Socket         (socket)
import           Metro.Utils             (setupLog)
import           System.Log              (Priority (..))
import           System.Log.Logger       (errorM)
import           UnliftIO

data ProxyMode = LocalToRemote | RemoteToLocal
  deriving (Show, Eq)

data Config = Config
  { holeSockPort :: String
  , outSockPort  :: String
  , logLevel     :: Priority
  , proxyMode    :: ProxyMode
  , cryptoMethod :: String
  , cryptoCipher :: String
  , cryptoKey    :: String
  }

type HoleServerEnv serv = ServerEnv serv () ByteString Word16 Packet
type HoleNodeList tp = IOMap ByteString (HoleEnv tp)

newHoleServer
  :: (Servable serv, Transport tp)
  => (TransportConfig (STP serv) -> TransportConfig tp)
  -> ServerConfig serv
  -> HoleSessionT tp IO ()
  -> IO (Async (), HoleServerEnv serv tp)
newHoleServer mk config sess = do
  gen <- sessionGen 1 (maxBound - 1000)
  sEnv <- fmap mapEnv . initServerEnv config gen mk $ \_ _ connEnv -> do
    pkt <- runConnT connEnv receive
    return $ Just (getPacketData pkt, ())
  io <- async $ startServer sEnv sess
  return (io, sEnv)

  where mapEnv :: HoleServerEnv serv tp -> HoleServerEnv serv tp
        mapEnv = setServerName "Hole" . S.setSessionMode MultiAction

doAction
  :: (MonadUnliftIO m, Transport tp0, Transport tp1)
  => HoleNodeList tp0 -> TransportConfig tp1 -> m ()
doAction nodeList config = do
  nl <- mapM mapFunc =<< Map.elems nodeList
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

startHoleServerLR
  :: (Cipher cipher, BlockCipher cipher)
  => Maybe cipher -> Config -> IO ()
startHoleServerLR mcipher Config {..} = do
  setupLog logLevel

  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleServer" Raw
        m1 = debugConfig "OutServer" Raw

    startHoleServerLR_ (socketServer holeSockPort) (socketServer outSockPort) m0 m1
  else
    case mcipher of
      Nothing ->
        startHoleServerLR_ (socketServer holeSockPort) (socketServer outSockPort) id id
      Just cipher ->
        startHoleServerLR_
          (socketServer holeSockPort)
          (socketServer outSockPort)
          (makeCrypto cipher cryptoMethod cryptoKey)
          id

startHoleServerRL
  :: (Cipher cipher, BlockCipher cipher)
  => Maybe cipher -> Config -> IO ()
startHoleServerRL mcipher Config {..} = do
  setupLog logLevel
  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleServer" Raw
        m1 = debugConfig "OutClient" Raw

    startHoleServerRL_ (socketServer holeSockPort) (m1 $ socket outSockPort) m0
  else
    case mcipher of
      Nothing ->
        startHoleServerRL_ (socketServer holeSockPort) (socket outSockPort) id
      Just cipher ->
        startHoleServerRL_
          (socketServer holeSockPort)
          (socket outSockPort)
          (makeCrypto cipher cryptoMethod cryptoKey)


startHoleServer_ :: (Cipher cipher, BlockCipher cipher) => Maybe cipher -> Config -> IO ()
startHoleServer_ mcipher config = do
  case proxyMode config of
    LocalToRemote -> startHoleServerLR mcipher config
    RemoteToLocal -> startHoleServerRL mcipher config

startHoleServer :: Config -> IO ()
startHoleServer config =
  case cryptoCipher config of
    "aes128"      -> startHoleServer_ (Just (undefined :: AES128)) config
    "aes192"      -> startHoleServer_ (Just (undefined :: AES192)) config
    "aes256"      -> startHoleServer_ (Just (undefined :: AES256)) config

    "blowfish"    -> startHoleServer_ (Just (undefined :: Blowfish)) config
    "blowfish64"  -> startHoleServer_ (Just (undefined :: Blowfish64)) config
    "blowfish128" -> startHoleServer_ (Just (undefined :: Blowfish128)) config
    "blowfish256" -> startHoleServer_ (Just (undefined :: Blowfish256)) config
    "blowfish448" -> startHoleServer_ (Just (undefined :: Blowfish448)) config

    "cast5"       -> startHoleServer_ (Just (undefined :: CAST5)) config

    "camellia128" -> startHoleServer_ (Just (undefined :: Camellia128)) config

    "des"         -> startHoleServer_ (Just (undefined :: DES)) config

    "des_eee3"    -> startHoleServer_ (Just (undefined :: DES_EEE3)) config
    "des_ede3"    -> startHoleServer_ (Just (undefined :: DES_EDE3)) config
    "des_eee2"    -> startHoleServer_ (Just (undefined :: DES_EEE2)) config
    "des_ede2"    -> startHoleServer_ (Just (undefined :: DES_EDE2)) config

    "twofish128"  -> startHoleServer_ (Just (undefined :: Twofish128)) config
    "twofish192"  -> startHoleServer_ (Just (undefined :: Twofish192)) config
    "twofish256"  -> startHoleServer_ (Just (undefined :: Twofish256)) config

    "none"        -> startHoleServer_ (Nothing :: Maybe AES128) config
    method        -> error $ "Error: " ++ method ++ "not support"

startHoleClient_
  :: (Cipher cipher, BlockCipher cipher)
  => Maybe cipher -> ByteString -> Config -> IO ()
startHoleClient_ mcipher cid config =
  case proxyMode config of
    LocalToRemote -> startHoleClientLR mcipher cid config
    RemoteToLocal -> startHoleClientRL mcipher cid config

startHoleClient
  :: ByteString -> Config -> IO ()
startHoleClient cid config =
  case cryptoCipher config of
    "aes128"      -> startHoleClient_ (Just (undefined :: AES128)) cid config
    "aes192"      -> startHoleClient_ (Just (undefined :: AES192)) cid config
    "aes256"      -> startHoleClient_ (Just (undefined :: AES256)) cid config

    "blowfish"    -> startHoleClient_ (Just (undefined :: Blowfish)) cid config
    "blowfish64"  -> startHoleClient_ (Just (undefined :: Blowfish64)) cid config
    "blowfish128" -> startHoleClient_ (Just (undefined :: Blowfish128)) cid config
    "blowfish256" -> startHoleClient_ (Just (undefined :: Blowfish256)) cid config
    "blowfish448" -> startHoleClient_ (Just (undefined :: Blowfish448)) cid config

    "cast5"       -> startHoleClient_ (Just (undefined :: CAST5)) cid config

    "camellia128" -> startHoleClient_ (Just (undefined :: Camellia128)) cid config

    "des"         -> startHoleClient_ (Just (undefined :: DES)) cid config

    "des_eee3"    -> startHoleClient_ (Just (undefined :: DES_EEE3)) cid config
    "des_ede3"    -> startHoleClient_ (Just (undefined :: DES_EDE3)) cid config
    "des_eee2"    -> startHoleClient_ (Just (undefined :: DES_EEE2)) cid config
    "des_ede2"    -> startHoleClient_ (Just (undefined :: DES_EDE2)) cid config

    "twofish128"  -> startHoleClient_ (Just (undefined :: Twofish128)) cid config
    "twofish192"  -> startHoleClient_ (Just (undefined :: Twofish192)) cid config
    "twofish256"  -> startHoleClient_ (Just (undefined :: Twofish256)) cid config
    "none"   -> startHoleClient_ (Nothing :: Maybe AES256) cid config
    method   -> error $ "Error: " ++ method ++ "not support"

startHoleClientLR
  :: (Cipher cipher, BlockCipher cipher)
  => Maybe cipher -> ByteString -> Config -> IO ()
startHoleClientLR mcipher cid Config {..} = do
  setupLog logLevel

  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleClient" Raw
        m1 = debugConfig "OutClient" Raw
    runHoleClientLR cid (m0 $ socket holeSockPort) (m1 $ socket outSockPort)
  else
    case mcipher of
      Nothing -> runHoleClientLR cid (socket holeSockPort) (socket outSockPort)
      Just cipher ->
        runHoleClientLR cid
          (makeCrypto cipher cryptoMethod cryptoKey $ socket holeSockPort)
          (socket outSockPort)

startHoleClientRL
  :: (Cipher cipher, BlockCipher cipher)
  => Maybe cipher -> ByteString -> Config -> IO ()
startHoleClientRL mcipher cid Config {..} = do
  setupLog logLevel
  if logLevel == DEBUG then do
    let m0 = debugConfig "HoleClient" Raw
        m1 = debugConfig "OutServer" Raw
    runHoleClientRL cid (m0 $ socket holeSockPort) (socketServer outSockPort) m1
  else
    case mcipher of
      Nothing ->
        runHoleClientRL cid (socket holeSockPort) (socketServer outSockPort) id
      Just cipher ->
        runHoleClientRL cid
          (makeCrypto cipher cryptoMethod cryptoKey $ socket holeSockPort)
          (socketServer outSockPort)
          id

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
  runConnT connEnv $ send $ packet Trns cid
  env0 <- initHoleEnv connEnv cid
  io <- async $ startHoleT env0 sess
  return (io, env0)
