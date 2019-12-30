{-# LANGUAGE RecordWildCards #-}

module Main where

import           Hole
import           Options.Applicative

data Flags = Flags
  { flagHoleAddr :: String
  , flagAddr     :: String
  , flagRL       :: Bool
  , flagLog      :: String
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
    <> help "Log level"
    <> value "INFO")

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
    { holeSockPort = flagHoleAddr
    , outSockPort = flagAddr
    , logLevel = read flagLog
    , proxyMode = if flagRL then RemoteToLocal else LocalToRemote
    }
