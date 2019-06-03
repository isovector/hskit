{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.RPC where

import           Control.Concurrent.MVar
import           Control.IPC
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Int (Int32)
import           Network.Socket (Socket, AddrInfo)
import           Polysemy
import           Polysemy.Input


data RPC m a where
  SendMessage :: ByteString -> RPC m ()
  RecvMessage :: RPC m ByteString

makeSem ''RPC


sendSomething :: (Member RPC r, Binary a) => a -> Sem r ()
sendSomething = sendMessage . BL.toStrict . runPut . put

recvSomething :: (Member RPC r, Binary a) => Sem r a
recvSomething = fmap (runGet get . BL.fromStrict) recvMessage


getPort :: IO Int32
getPort = pure 6112

getAddr :: Int32 -> IO AddrInfo
getAddr = netAddr . show

getHostSocket :: Int32 -> IO (MVar Socket)
getHostSocket = (netHost =<<) . getAddr

getClientSocket :: Int32 -> IO Socket
getClientSocket = (netClient =<<) . getAddr


-- | This is roundabout because we need to wait until the MVar is definitely full
runRPCOverUDP
    :: Member (Lift IO) r
    => MVar Socket
    -> Sem (RPC ': r) a
    -> Sem r a
runRPCOverUDP mvar
  = runMonadicInput (sendM $ readMVar mvar)
  . reinterpret \case
      SendMessage msg -> input >>= sendM . flip netSend msg
      RecvMessage     -> input >>= sendM . netRecv

runRPCOverUDP'
    :: Member (Lift IO) r
    => Socket
    -> Sem (RPC ': r) a
    -> Sem r a
runRPCOverUDP' socket =
  interpret
    \case
      SendMessage msg -> sendM $ netSend socket msg
      RecvMessage     -> sendM $ netRecv socket


