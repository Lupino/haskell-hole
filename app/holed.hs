{-# LANGUAGE RecordWildCards #-}

module Main where

import           Hole
import           Options.Applicative

data Flags = Flags
  { flagHoleAddr :: String
  , flagOutAddr  :: String
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
    (  long "out-addr"
    <> short 'o'
    <> metavar "ADDRESS"
    <> help "Out address"
    <> value "tcp://127.0.0.1:4001")
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
    , outSockPort = flagOutAddr
    , logLevel = read flagLog
    }
