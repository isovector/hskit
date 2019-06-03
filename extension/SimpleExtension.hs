{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SimpleExtension where

import           Control.Concurrent
import           Control.Monad
import           Data.GI.Base (newObject, on, GVariant)
import           Data.GI.Base.GVariant (newGVariantFromPtr, fromGVariant)
import           Data.IORef
import           Foreign.Ptr (Ptr)
import qualified GI.WebKit2WebExtension as WE
import           Polysemy
import           Polysemy.Labeler
import           Polysemy.Error
import           Polysemy.NonDet
import           Polysemy.RPC


foreign export ccall initialize_simple_web_extension_with_user_data :: Ptr WE.WebExtension -> Ptr GVariant -> IO ()

initialize_simple_web_extension_with_user_data
    :: Ptr WE.WebExtension
    -> Ptr GVariant
    -> IO ()
initialize_simple_web_extension_with_user_data extensionPtr dataPtr = do
  extension <- newObject WE.WebExtension extensionPtr
  userData <- newGVariantFromPtr dataPtr

  Just port <- fromGVariant userData
  socket    <- getClientSocket port

  ref <- newIORef undefined
  void $ on extension #pageCreated $ \page -> do
    void $ on page #documentLoaded $ do
      Just dom <- #getDomDocument page
      win <- #getDefaultView dom
      writeIORef ref win

  void
      . forkIO
      . forever
      . runM
      . runNonDet @Maybe
      . runError
      . runRPCOverUDP' socket
      . runLabelerWebExt ref
      $ dispatchLabeler

