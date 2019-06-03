{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.RPC where

import Control.Concurrent.MVar
import Control.IPC
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Network.Socket (Socket, AddrInfo)
import Polysemy


data RPC m a where
  SendMessage :: ByteString -> RPC m ()
  RecvMessage :: RPC m ByteString

makeSem ''RPC


getPort :: IO Int32
getPort = pure 6112

getAddr :: Int32 -> IO AddrInfo
getAddr = netAddr . show

getHostSocket :: Int32 -> IO (MVar Socket)
getHostSocket = (netHost =<<) . getAddr

getClientSocket :: Int32 -> IO Socket
getClientSocket = (netClient =<<) . getAddr


runRPCOverUDP
    :: Member (Lift IO) r
    => MVar Socket
    -> Sem (RPC ': r) a
    -> Sem r a
runRPCOverUDP mvar m = do
  socket <- sendM $ readMVar mvar
  interpret
    \case
      SendMessage msg -> sendM $ netSend socket msg
      RecvMessage     -> sendM $ netRecv socket
    m

