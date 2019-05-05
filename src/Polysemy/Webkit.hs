{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Polysemy.Webkit where

import           Control.Monad
import           Data.Char (chr)
import           Data.Foldable
import           Data.GI.Base
import           Data.Text
import           GI.Gdk.Structs.EventKey
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2.Objects.WebView as WV
import           Polysemy
import GI.Gio.Objects.Cancellable (noCancellable)


data Webkit m a where
  NavigateTo     :: Text -> Webkit m ()
  InstallKeyHook :: (Char -> m Bool) -> Webkit m ()
  ScrollDown     :: Webkit m ()
  ScrollUp       :: Webkit m ()

makeSem ''Webkit


runWebkit'
    :: Member (Lift IO) r
    => WV.WebView
    -> (forall x. Sem r x -> IO x)
    -> Sem (Webkit ': r) a
    -> Sem r a
runWebkit' wv lower = interpretH \case
  NavigateTo url -> do
    WV.webViewLoadUri wv url
    getInitialStateT

  ScrollDown -> do
    sendM $ WV.webViewRunJavascript wv "window.scrollBy(0, 14 * 3);" noCancellable Nothing
    getInitialStateT

  ScrollUp -> do
    sendM $ WV.webViewRunJavascript wv "window.scrollBy(0, -14 * 3);" noCancellable Nothing
    getInitialStateT

  InstallKeyHook hook -> do
    h      <- bindT hook
    istate <- getInitialStateT

    sendM $ after wv #keyPressEvent $ \kp -> do
      kv   <- get kp #keyval
      (lower .@ runWebkit' wv) $ h $ chr (fromIntegral kv) <$ istate
      pure False

    pure istate


runWebkit
    :: Member (Lift IO) r
    => Gtk.Window
    -> (forall x. Sem r x -> IO x)
    -> Sem (Webkit ': r) a
    -> Sem r a
runWebkit win lower m = do
  wv <- sendM $ new WV.WebView []
  a <- runWebkit' wv lower m

  #add win wv
  pure a

-- settings >
-- GI.WebKit2.Objects.WebsiteDataManager.websiteDataManagerGetCookieManager
-- > set persistant storage

showWin :: IO ()
showWin = do
  Gtk.init Nothing
  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit

  (runM .@ runWebkit win) $ do
    installKeyHook $ \c -> do
      sendM $ print c
      case c of
        'j' -> scrollDown
        'k' -> scrollUp
        _   -> pure ()
      pure True
    navigateTo "http://github.com"

  #showAll win
  Gtk.main

