{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.Char           (toLower, toUpper)
import           Data.List           (isPrefixOf)
import           Hole
import           Hole.Types          (PacketType (..), formatMessage)
import           Options.Applicative

data Flags = Flags
  { flagHoleAddr  :: String
  , flagAddr      :: String
  , flagRL        :: Bool
  , flagLog       :: String
  , flagMethod    :: String
  , flagCipher    :: String
  , flagKey       :: String
  , flagPeer      :: String
  , flagPeerAlive :: Int
  , flagName      :: String
  }

parser :: Parser Flags
parser = Flags
  <$> strOption
    (  long "hole-addr"
    <> short 'H'
    <> metavar "ADDRESS"
    <> help "Hole address"
    <> value "tcp://127.0.0.1:4000")
  <*> strOption
    (  long "addr"
    <> short 'a'
    <> metavar "ADDRESS"
    <> help "Address"
    <> value "tcp://127.0.0.1:4001")
  <*> switch
    (  long "use-remote-to-local"
    <> help "Use remote to local mode, default is local to remote")
  <*> strOption
    (  long "log-level"
    <> short 'l'
    <> metavar "LEVEL"
    <> help "Log level. support DEBUG INFO NOTICE WARNING ERROR CRITICAL ALERT EMERGENCY"
    <> value "INFO")
  <*> strOption
    (  long "method"
    <> short 'm'
    <> metavar "METHOD"
    <> help "Crypto method. support cbc cfb ecb ctr. default cfb"
    <> value "cfb")
  <*> strOption
    (  long "cipher"
    <> short 'c'
    <> metavar "CIPHER"
    <> help "Crypto cipher. support aes128 aes192 aes256 blowfish blowfish64 blowfish128 blowfish256 blowfish448 cast5 camellia128 des des_eee3 des_ede3 des_eee2 des_ede2 twofish128 twofish192 twofish256 none. default none"
    <> value "none")
  <*> strOption
    (  long "key"
    <> short 'k'
    <> metavar "KEY"
    <> help "Crypto key."
    <> value "none")
  <*> strOption
    (  long "peer"
    <> short 'p'
    <> metavar "PEER"
    <> help "Peer Server."
    <> value "")
  <*> option auto
    (  long "peer-alive"
    <> metavar "PEER ALIVE"
    <> help "Peer Keepalive."
    <> value 600)
  <*> strOption
    (  long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "Peer Name"
    <> value "peer-name")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Hole Server"
     <> header "holed - Hole Server" )

program :: Flags -> IO ()
program Flags {..} =
  startHoleServer Config
    { holeSockPort = flagHoleAddr ++ holeInfo
    , outSockPort  = flagAddr
    , logLevel     = read $ map toUpper flagLog
    , proxyMode    = if flagRL then RemoteToLocal else LocalToRemote
    , cryptoMethod = map toLower flagMethod
    , cryptoCipher = map toLower flagCipher
    , cryptoKey    = flagKey
    }

  where holeInfo =
          if "udp" `isPrefixOf` flagHoleAddr then
            "?peer=" ++ flagPeer ++ "&keepalive=" ++ show flagPeerAlive ++ "&message=" ++ formatMessage PeerReg flagName
          else ""
