{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Polysemy.Webkit where

import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Monad
import           Data.Char (chr)
import           Data.Foldable
import           Data.GI.Base
import           Data.GI.Gtk.Threading
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Trie as T
import qualified GI.Gdk as Gdk
import           GI.Gio.Objects.Cancellable (noCancellable)
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK2
import           Polysemy
import           Polysemy.Operators
import qualified Polysemy.State as S
import           Polysemy.State hiding (get)


data Delayed m a where
  Delay :: Int -> m () -> Delayed m ()
  Cancel :: Delayed m ()

makeSem ''Delayed


data WebView m a where
  GetWebView :: WebView m (WK2.WebView)

makeSem ''WebView

data Viewport m a where
  ScrollTop :: Viewport m ()
  ScrollDown :: Viewport m ()
  ScrollUp   :: Viewport m ()
  ScrollBottom :: Viewport m ()

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

runDelayed
    :: (forall x. r@> x -> IO x)
    -> Delayed :r@> a
    -> IO ~@r@> a
runDelayed lower m = do
  ref <- sendM $ newIORef []
  runDelayed' ref lower m


-- TODO(sandy): this crashes like crazy
runDelayed'
    :: IORef ([A.Async ()])
    -> (forall x. r@> x -> IO x)
    -> Delayed :r@> a
    -> IO ~@r@> a
runDelayed' ref lower = interpretH \case
  Cancel -> do
    sendM $ do
      mfuture <- readIORef ref
      for_ mfuture $ \e -> do
        A.cancel e
      modifyIORef ref $ drop 1
    getInitialStateT

  Delay time m -> do
    m' <- runT m
    sendM $ do
      a <- A.async $ do
        threadDelay time
        postGUIASync $ void $ lower .@ runDelayed' ref $ m'
        modifyIORef ref $ drop 1
      modifyIORef ref $ (++ [a])
    getInitialStateT


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
  ScrollTop -> do
    wv <- getWebView
    sendM $ WK2.webViewRunJavascript wv "window.scrollTo(0, 0);" noCancellable Nothing

  ScrollDown -> do
    wv <- getWebView
    sendM $ WK2.webViewRunJavascript wv "window.scrollBy(0, 14 * 3);" noCancellable Nothing

  ScrollUp -> do
    wv <- getWebView
    sendM $ WK2.webViewRunJavascript wv "window.scrollBy(0, -14 * 3);" noCancellable Nothing

  ScrollBottom -> do
    wv <- getWebView
    sendM $ WK2.webViewRunJavascript wv "window.scrollTo(0, 9999999);" noCancellable Nothing


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


keycommands
    :: forall r a
     . T.Trie Char (r@> ())
    -> '[Lift IO, Navigation, Delayed] >@r@> ()
keycommands t = do
  sref <- sendM $ newIORef ""
  tref <- sendM $ newIORef t
  runStateInIORef sref
    . runStateInIORef tref
    . installKeyHook
    $ \chr -> do
      t' <- S.get @(T.Trie Char (r@> ()))
      case T.follow t' chr of
        (Nothing, Just f) -> do
          cancel
          raise $ raise f
          S.put t
        (Nothing, Nothing) -> do
          cancel
          S.put t
        (Just t', Just f) -> do
          delay 1000000 $ do
            raise $ raise f
            S.put t
          S.put t'
        (Just t', Nothing) -> do
          cancel
          S.put t'
      pure True

-- settings >
-- GI.WebKit2.Objects.WebsiteDataManager.websiteDataManagerGetCookieManager
-- > set persistant storage

showWin :: IO ()
showWin = do
  Gtk.init Nothing
  setCurrentThreadAsGUIThread

  win <- new Gtk.Window [ #title := "Hi there" ]
  on win #destroy Gtk.mainQuit

  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  wv   <- new WK2.WebView [ #vexpand := True ]

  uriEntry <- new Gtk.Entry
    [ #placeholderText := "Type the address to load here"
    , #widthChars := 50
    ]

-- TODO(sandy): BUG BUG BUG BUG BUG
-- because runDelayed spins up an IORef, this fucks everything
  ((runM . runWebView wv) .@ runNavigation .@ runDelayed .@ runFreeForm uriEntry) . runViewport $ do
    keycommands $ T.fromList
      [ ("j", scrollDown)
      , ("k", scrollUp)
      , ("H", goBackward)
      , ("L", goForward)
      , ("o", withFreeForm "https://" $ \uri -> navigateTo uri)
      , ("oo", withFreeForm "COOL" $ \uri -> navigateTo uri)
      , ("gg", scrollTop)
      , ("G", scrollBottom)
      ]

    navigateTo "http://github.com"

  #add vbox wv
  #add vbox uriEntry

  #add win vbox
  #showAll win
  Gtk.main


esc :: Char
esc = '\65307'

