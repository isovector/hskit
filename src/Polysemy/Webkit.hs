{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Polysemy.Webkit where

import           Control.Monad
import           Data.GI.Base
import           Data.GI.Gtk.Threading
import           Data.IORef
import qualified Data.Trie as T
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK2
import           Javascript
import           Polysemy
import           Polysemy.Delayed
import           Polysemy.FreeForm
import           Polysemy.IdempotentLowering
import           Polysemy.Input
import           Polysemy.Navigation
import           Polysemy.Operators
import qualified Polysemy.State as S
import           Polysemy.State hiding (get)
import           Polysemy.Viewport


keycommands
    :: forall r
     . T.Trie Char (r@> ())
    -> '[Lift IO, Navigation, Delayed] >@r@> ()
keycommands t = do
  sref <- sendM $ newIORef ("" :: String)
  tref <- sendM $ newIORef t
  runStateInIORef sref
    . runStateInIORef tref
    . installKeyHook
    $ \c -> do
      t' <- S.get @(T.Trie Char (r@> ()))
      case T.follow t' c of
        (Nothing, Just f) -> do
          cancel
          raise $ raise f
          S.put t
        (Nothing, Nothing) -> do
          cancel
          S.put t
        (Just _, Just f) -> do
          delay 1000000 $ do
            raise $ raise f
            S.put t
          S.put t'
        (Just t'', Nothing) -> do
          cancel
          S.put t''
      pure True

-- settings >
-- GI.WebKit2.Objects.WebsiteDataManager.websiteDataManagerGetCookieManager
-- > set persistant storage

showWin :: IO ()
showWin = do
  void $ Gtk.init Nothing
  setCurrentThreadAsGUIThread

  win <- new Gtk.Window [ #title := "Hi there" ]
  void $ on win #destroy Gtk.mainQuit

  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]
  wv   <- new WK2.WebView [ #vexpand := True ]

  uriEntry <- new Gtk.Entry
    [ #placeholderText := "Type the address to load here"
    , #widthChars := 50
    ]

  handler <- nat (runM . runConstInput wv .@ runNavigation)
         .@! runDelayed
         .@! runFreeForm uriEntry wv

  handler . runViewport $ do
    keycommands $ T.fromList
      [ ("j", scrollDown)
      , ("k", scrollUp)
      , ("H", goBackward)
      , ("L", goForward)
      , ("o", withFreeForm "https://" $ \uri -> navigateTo uri)
      , ("f", sendM $ runJS wv jsHinting)
      , ("gg", scrollTop)
      , ("G", scrollBottom)
      ]

    navigateTo "http://github.com/isovector/hskit"

  #add vbox wv
  #add vbox uriEntry

  #add win vbox
  #showAll win
  Gtk.main


esc :: Char
esc = '\65307'

