
--   uriEntry <- new Gtk.Entry [#placeholderText := "Type the address to load here",
--                              #widthChars := 50]
--   on uriEntry #activate $ do
--     uri <- uriEntry `get` #text
--     #loadUri view uri

--   header <- new Gtk.HeaderBar [#showCloseButton := True,
--                                #customTitle := uriEntry,
--                                #title := "A simple WebKit browser"]
--   #setTitlebar win (Just header)

--   on view (PropertyNotify #estimatedLoadProgress) $ \_ -> do
--     status <- view `get` #estimatedLoadProgress
--     uriEntry `set` [#progressFraction := if status /= 1.0
--                                          then status
--                                          else 0]


{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Polysemy.Webkit where

import           Control.Monad
import           Data.Char (chr)
import           Data.Foldable
import           Data.GI.Base
import           Data.Text
import qualified GI.Gdk as Gdk
import           GI.Gio.Objects.Cancellable (noCancellable)
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK2
import           Polysemy
import           Polysemy.Operators

data WebView m a where
  GetWebView :: WebView m (WK2.WebView)

makeSem ''WebView

data Viewport m a where
  ScrollDown :: Viewport m ()
  ScrollUp   :: Viewport m ()

makeSem ''Viewport

data Webkit m a where
  NavigateTo     :: Text -> Webkit m ()
  GoForward      :: Webkit m ()
  GoBackward     :: Webkit m ()
  InstallKeyHook :: (Char -> m Bool) -> Webkit m ()

makeSem ''Webkit

runWebView
    :: WK2.WebView
    -> WebView :r@> a
    -> IO ~@r@> a
runWebView wv = interpret \case
  GetWebView -> pure wv


runViewport
  :: Viewport :r@> a
  -> '[Lift IO, WebView] >@r@> a
runViewport = interpret \case
  ScrollDown -> do
    wv <- getWebView
    sendM $ WK2.webViewRunJavascript wv "window.scrollBy(0, 14 * 3);" noCancellable Nothing

  ScrollUp -> do
    wv <- getWebView
    sendM $ WK2.webViewRunJavascript wv "window.scrollBy(0, -14 * 3);" noCancellable Nothing


runWebkit
    :: (forall x. r@> x -> IO x)
    -> Webkit :r@> a
    -> '[Lift IO, WebView] >@r@> a
runWebkit lower = interpretH \case
  NavigateTo url -> do
    wv <- getWebView
    WK2.webViewLoadUri wv url
    getInitialStateT

  GoForward -> do
    wv <- getWebView
    #goForward wv
    getInitialStateT

  GoBackward -> do
    wv <- getWebView
    #goBack wv
    getInitialStateT

  InstallKeyHook hook -> do
    wv     <- getWebView
    h      <- bindT hook
    istate <- getInitialStateT

    sendM $ after wv #keyPressEvent $ \kp -> do
      kv   <- get kp #keyval
      (lower .@ runWebkit) $ h $ chr (fromIntegral kv) <$ istate
      pure False

    pure istate


-- settings >
-- GI.WebKit2.Objects.WebsiteDataManager.websiteDataManagerGetCookieManager
-- > set persistant storage

showWin :: IO ()
showWin = do
  Gtk.init Nothing

  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit

  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  wv   <- new WK2.WebView [ #vexpand := True ]

  uriEntry <- new Gtk.Entry
    [ #placeholderText := "Type the address to load here"
    , #widthChars := 50
    ]

  ((runM . runWebView wv) .@ runWebkit) . runViewport $ do
    installKeyHook $ \c -> do
      sendM $ print c
      case c of
        'j' -> scrollDown
        'k' -> scrollUp
        'H' -> goBackward
        'L' -> goForward
        'o' -> navigateTo "http://github.com"
        _   -> pure ()
      pure True
    navigateTo "http://github.com"

  #add vbox wv
  #add vbox uriEntry

  #add win vbox
  #showAll win
  Gtk.main

main = showWin

