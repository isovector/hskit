{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Monad
import           Data.Bifunctor
import           Data.GI.Base
import           Data.GI.Gtk.Threading
import qualified Data.Trie as T
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK2
import           Javascript
import           Polysemy
import           Polysemy.Delayed
import           Polysemy.FreeForm
import           Polysemy.IdempotentLowering
import           Polysemy.Input
import           Polysemy.Labeler
import           Polysemy.Navigation
import           Polysemy.RPC
import           Polysemy.Viewport
import           Polysemy.Webkit

-- settings >
-- GI.WebKit2.Objects.WebsiteDataManager.websiteDataManagerGetCookieManager
-- > set persistant storage

main :: IO ()
main = do
  void $ Gtk.init Nothing
  setCurrentThreadAsGUIThread

  port <- getPort
  mvar <- getHostSocket port

  win <- new Gtk.Window [ #title := "Hi there" ]
  void $ on win #destroy Gtk.mainQuit

  vbox <- new Gtk.Box [ #orientation := Gtk.OrientationVertical ]

  context <- new WK2.WebContext []
  void $ on context #initializeWebExtensions $ do
    #setWebExtensionsDirectory
        context
        ".stack-work/install/x86_64-linux-tinfo6/lts-13.0/8.6.3/lib/"
    userData <- toGVariant port
    #setWebExtensionsInitializationUserData context userData

  wv   <- new WK2.WebView [ #vexpand := True, #webContext := context ]

  uriEntry <- new Gtk.Entry
    [ #placeholderText := "Type the address to load here"
    , #widthChars := 50
    ]

  handler <- nat (runM @IO . runRPCOverUDP mvar . runConstInput wv .@ runNavigation)
         .@! runDelayed
         .@! runFreeForm uriEntry wv

  handler . runViewport . runLabelerOverRPC $ do
    keycommands $ T.fromList
      [ ("j", scrollDown)
      , ("k", scrollUp)
      , ("H", goBackward)
      , ("L", goForward)
      , ("o", withFreeForm "https://" $ \uri -> navigateTo uri)
      , ("f", sendM $ runJS wv jsHinting)
      , ("gg", scrollTop)
      , ("G", scrollBottom)
      , ("p", do
                rects <- getLinkRects
                setLabels $ fmap (first pure) $ zip ['a' ..] rects
        )
      ]

    navigateTo "http://github.com/isovector/hskit"

  #add vbox wv
  #add vbox uriEntry

  #add win vbox
  #showAll win
  Gtk.main



