{-# LANGUAGE RecordWildCards #-}

module Main where

import           Data.String         (fromString)
import           Hole
import           Options.Applicative

data Flags = Flags
  { flagHoleAddr :: String
  , flagInAddr   :: String
  , flagLog      :: String
  , flagCID      :: String
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
    (  long "in-addr"
    <> short 'i'
    <> metavar "ADDRESS"
    <> help "Int address"
    <> value "tcp://127.0.0.1:80")
  <*> strOption
    (  long "log-level"
    <> short 'l'
    <> metavar "LEVEL"
    <> help "Log level"
    <> value "INFO")
  <*> strOption
    (  long "name"
    <> short 'n'
    <> metavar "NAME"
    <> help "Client name"
    <> value "hole-client")

main :: IO ()
main = execParser opts >>= program
  where
    opts = info (helper <*> parser)
      ( fullDesc
     <> progDesc "Hole Client"
     <> header "hole - Hole Client" )

program :: Flags -> IO ()
program Flags {..} =
  startHoleClient (fromString flagCID) Config
    { holeSockPort = flagHoleAddr
    , outSockPort = flagInAddr
    , logLevel = read flagLog
    }
