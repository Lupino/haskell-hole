module Main where


import           Control.Monad         (forever)
import qualified Data.ByteString.Char8 as B (putStr)
import           Hole.Types            (Packet (..))
import           Metro.Conn            (initConnEnv, receive, runConnT)
import           Metro.Socket          (bindTo)
import           Metro.TP.BS           (bsTPConfig, newBSHandle)
import           Metro.TP.UDPSocket    (recvFrom)
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
                doSendAll (getSocket serv) (addrAddress addrInfo) . B.pack $ formatMessage NatEcho $ show addr
          _ -> pure ()
