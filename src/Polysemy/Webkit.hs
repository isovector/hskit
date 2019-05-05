
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

module Polysemy.Navigation where

import           Control.Monad
import           Data.Char (chr)
import           Data.Foldable
import           Data.GI.Base
import           Data.IORef
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

data Navigation m a where
  NavigateTo     :: Text -> Navigation m ()
  GoForward      :: Navigation m ()
  GoBackward     :: Navigation m ()
  InstallKeyHook :: (Char -> m Bool) -> Navigation m ()

makeSem ''Navigation

data FreeForm m a where
  WithFreeForm :: Text -> (Text -> m ()) -> FreeForm m ()

makeSem ''FreeForm

runFreeForm'
    :: forall r a
     . Gtk.Entry
    -> IORef (Text -> Sem (FreeForm ': r) ())
    -> (forall x. r@> x -> IO x)
    -> FreeForm :r@> a
    -> IO ~@r@> a
runFreeForm' entry ref lower = interpretH \case
  WithFreeForm prompt f -> do
    f' <- bindT f
    is <- getInitialStateT
    sendM $ writeIORef ref $ \t -> void $ f' $ t <$ is
    Gtk.widgetGrabFocus entry
    set entry
      [ #text := prompt
      ]
    #setPosition entry 65535
    getInitialStateT

runFreeForm
    :: Gtk.Entry
    -> (forall x. r@> x -> IO x)
    -> FreeForm :r@> a
    -> '[Lift IO, WebView] >@r@> a
runFreeForm entry lower m = do
  wv <- getWebView
  ref <- sendM $ newIORef $ const $ pure ()
  sendM $ on entry #activate $ do
    Gtk.widgetGrabFocus wv
    t <- get entry #text
    set entry
      [ #text := ""
      ]
    f <- readIORef ref
    lower .@ runFreeForm' entry ref $ f t
  runFreeForm' entry ref lower m


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


runNavigation
    :: (forall x. r@> x -> IO x)
    -> Navigation :r@> a
    -> '[Lift IO, WebView] >@r@> a
runNavigation lower = interpretH \case
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
      lower .@ runNavigation $ h $ chr (fromIntegral kv) <$ istate
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

  ((runM . runWebView wv) .@ runNavigation .@ runFreeForm uriEntry) . runViewport $ do
    installKeyHook $ \c -> do
      sendM $ print c
      case c of
        'j' -> scrollDown
        'k' -> scrollUp
        'H' -> goBackward
        'L' -> goForward
        'o' -> withFreeForm "https://" $ \uri -> navigateTo uri
        _   -> pure ()
      pure True
    navigateTo "http://github.com"

  #add vbox wv
  #add vbox uriEntry

  #add win vbox
  #showAll win
  Gtk.main

main = showWin

