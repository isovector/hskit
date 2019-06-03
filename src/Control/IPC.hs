-- from https://gist.github.com/k3karthic/ef4f7141a9506d1d7ee50ba4f75127a1

module Control.IPC where

import Control.Concurrent.MVar
import           Control.Concurrent
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int
import           Network.Socket hiding (recvFrom)
import           Network.Socket.ByteString


netHost :: AddrInfo -> IO (MVar Socket)
netHost addr = do
  mvar <- newEmptyMVar
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (addrAddress addr)
  let fd = fdSocket sock
  setCloseOnExecIfNeeded fd
  listen sock 10
  void $ forkIO $ do
    z <- accept sock
    putMVar mvar $ fst z
  pure mvar


netClient :: AddrInfo -> IO Socket
netClient addr = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock $ addrAddress addr
  pure sock


netAddr :: String -> IO AddrInfo
netAddr port = do
    addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just port)
    pure $ head addrinfos


netRecv :: Socket -> IO B.ByteString
netRecv sock = do
    value_length' <- fst <$> recvFrom sock 4
    let value_length'' = BL.fromStrict value_length'
    let value_length = fromIntegral (runGet parseInt32 value_length'') :: Int
    msg <- fst <$> recvFrom sock value_length
    pure msg
  where
    parseInt32 = get :: Get Int32


netSend :: Socket -> B.ByteString -> IO ()
netSend sock value = do
    sendAll sock value_length'
    sendAll sock value

  where
    value_length = fromIntegral (B.length value ) :: Int32
    value_length' = BL.toStrict $ runPut $ put $ value_length

