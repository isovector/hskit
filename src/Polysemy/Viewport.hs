{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Polysemy.Viewport where

import qualified GI.WebKit2 as WK2
import           Javascript
import           Polysemy
import           Polysemy.Input
import           Polysemy.Operators


data Viewport m a where
  ScrollTop    :: Viewport m ()
  ScrollDown   :: Viewport m ()
  ScrollUp     :: Viewport m ()
  ScrollBottom :: Viewport m ()

makeSem ''Viewport


runViewport
  :: Viewport :r@> a
  -> '[Lift IO, Input WK2.WebView] >@r@> a
runViewport = interpret \case
  ScrollTop -> do
    wv <- input
    sendM $ runJS wv jsScrollTop

  ScrollDown -> do
    wv <- input
    sendM $ runJS wv $ jsScroll 3

  ScrollUp -> do
    wv <- input
    sendM $ runJS wv $ jsScroll $ -3

  ScrollBottom -> do
    wv <- input
    sendM $ runJS wv jsScrollBottom

