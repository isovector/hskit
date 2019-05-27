{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

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
import           Polysemy.IdempotentLowering
import           Polysemy.ZooAdditions

data FreeForm m a where
  WithFreeForm :: Text -> (Text -> m ()) -> FreeForm m ()

makeSem ''FreeForm


runFreeForm
    :: ( Member (Lift IO) r
       )
    => Gtk.Entry
    -> WK2.WebView
    -> (forall x. r@> x -> IO x)
    -> IO (forall a. Sem (FreeForm ': r) a -> Sem r a)
runFreeForm entry wv lower = do
  ref <- newIORef $ const $ pure ()

  me <-
    fixedNat $ \me -> interpretH \case
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

  void $ on entry #activate $ do
    Gtk.widgetGrabFocus wv
    t <- get entry #text
    set entry
      [ #text := ""
      ]
    f <- readIORef ref
    lower $ me $ f t
  nat me

