{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SimpleExtension where

import Control.Concurrent
import           Control.IPC
import           Control.Monad
import           Data.GI.Base (newObject, on, get, GVariant)
import           Data.GI.Base.GVariant (newGVariantFromPtr, fromGVariant)
import           Data.Monoid ((<>))
import           Foreign.Ptr (Ptr)
import qualified GI.WebKit2WebExtension as WE
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
  socket <- getClientSocket port
  void $ forkIO $ forever $ do
    msg <- netRecv socket
    print msg

  void $ on extension #pageCreated $ \page -> do
    void $ on page #documentLoaded $ do
      uri <- page `get` #uri
      maybeDom <- #getDomDocument page
      maybeTitle <- case maybeDom of
        Just dom -> dom `get` #title
        Nothing -> return Nothing
      putStrLn $ "Loaded " <> show uri <> " with title "
        <> show maybeTitle <> "."

