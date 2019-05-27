{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Polysemy.Navigation where

import           Control.Monad
import           Data.Char (chr)
import           Data.GI.Base
import           Data.Text (Text)
import qualified GI.WebKit2 as WK2
import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators


data Navigation m a where
  NavigateTo     :: Text -> Navigation m ()
  GoForward      :: Navigation m ()
  GoBackward     :: Navigation m ()
  -- TODO(sandy): Why is this in Navigation?
  InstallKeyHook :: (Char -> m Bool) -> Navigation m ()

makeSem ''Navigation


runNavigation
    :: (forall x. r@> x -> IO x)
    -> Navigation :r@> a
    -> '[Lift IO, Input WK2.WebView] >@r@> a
runNavigation lower = interpretH \case
  NavigateTo url -> do
    wv <- input
    WK2.webViewLoadUri wv url
    getInitialStateT

  GoForward -> do
    wv <- input
    #goForward wv
    getInitialStateT

  GoBackward -> do
    wv <- input
    #goBack wv
    getInitialStateT

  InstallKeyHook hook -> do
    wv     <- input
    h      <- bindT hook
    istate <- getInitialStateT

    void $ sendM $ after wv #keyPressEvent $ \kp -> do
      kv   <- get kp #keyval
      void $ lower .@ runNavigation $ h $ chr (fromIntegral kv) <$ istate
      pure False

    pure istate

