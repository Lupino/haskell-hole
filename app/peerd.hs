module Main where


import           Control.Monad         (forever)
import qualified Data.ByteString.Char8 as B (pack, putStr, unpack)
import           Hole.Types            (Packet (..), PacketType (..),
                                        formatMessage)
import           Metro.Conn            (initConnEnv, receive, runConnT)
import           Metro.Socket          (bindTo, getDatagramAddr)
import           Metro.TP.BS           (bsTPConfig, newBSHandle)
import           Metro.TP.UDPSocket    (doSendAll, recvFrom)
import           Network.Socket        (addrAddress)
import           System.Environment    (getArgs)
import           UnliftIO


defaultPort :: String
defaultPort = "udp://0.0.0.0:0"

main :: IO ()
main = do
  args <- getArgs
  let port = case args of
                     []    -> defaultPort
                     (x:_) -> x

  sock <- bindTo port

  forever $ do
    (bs, addr) <- recvFrom sock 65535
    bsh <- newBSHandle bs
    let bstpc = bsTPConfig bsh (\_ -> pure ()) $ show addr
    connEnv <- initConnEnv bstpc

    mpkt <- timeout 1000000 $ tryAny $ runConnT connEnv receive
    case mpkt of
      Nothing         -> pure ()
      Just (Left err) -> print err
      Just (Right pkt) -> do
        case packetType pkt of
          PeerReg -> do
            putStr "Peer Client: "
            B.putStr $ packetData pkt
            putStr "@"
            putStr $ show addr
            putStrLn " connected"
          NatEcho -> do
            maddr <- getDatagramAddr $ B.unpack bs
            case maddr of
              Nothing -> pure ()
              Just addrInfo ->
                doSendAll sock (addrAddress addrInfo) . B.pack $ formatMessage NatEcho $ show addr
          _ -> pure ()
