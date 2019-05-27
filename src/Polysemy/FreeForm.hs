{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Polysemy.FreeForm where

import           Control.Monad
import           Data.GI.Base
import           Data.IORef
import           Data.Text (Text)
import qualified GI.Gtk as Gtk
import qualified GI.WebKit2 as WK2
import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators

data FreeForm m a where
  WithFreeForm :: Text -> (Text -> m ()) -> FreeForm m ()

makeSem ''FreeForm


runFreeForm'
    :: forall r a
     . Gtk.Entry
    -> IORef (Text -> Sem (FreeForm ': r) ())
    -> FreeForm :r@> a
    -> IO ~@r@> a
runFreeForm' entry ref = interpretH \case
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
    -> '[Lift IO, Input WK2.WebView] >@r@> a
runFreeForm entry lower m = do
  wv <- input
  ref <- sendM $ newIORef $ const $ pure ()
  void $ sendM $ on entry #activate $ do
    Gtk.widgetGrabFocus wv
    t <- get entry #text
    set entry
      [ #text := ""
      ]
    f <- readIORef ref
    lower . runFreeForm' entry ref $ f t
  runFreeForm' entry ref m

