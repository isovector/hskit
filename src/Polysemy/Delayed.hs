{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Polysemy.Delayed where

import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Monad
import           Data.Foldable
import           Data.GI.Gtk.Threading
import           Data.IORef
import           Polysemy
import           Polysemy.Operators


data Delayed m a where
  Delay :: Int -> m () -> Delayed m ()
  Cancel :: Delayed m ()

makeSem ''Delayed


runDelayed
    :: (forall x. r@> x -> IO x)
    -> Delayed :r@> a
    -> IO ~@r@> a
runDelayed lower m = do
  ref <- sendM $ newIORef []
  runDelayed' ref lower m


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

