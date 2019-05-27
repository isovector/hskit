{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Polysemy.Delayed where

import           Control.Concurrent
import qualified Control.Concurrent.Async as A
import           Control.Monad
import           Data.Foldable
import           Data.Function (fix)
import           Data.GI.Gtk.Threading
import           Data.IORef
import           Polysemy
import           Polysemy.Operators
import           Polysemy.ZooAdditions


data Delayed m a where
  Delay :: Int -> m () -> Delayed m ()
  Cancel :: Delayed m ()

makeSem ''Delayed


runDelayed
    :: Member (Lift IO) r
    => (forall x. r@> x -> IO x)
    -> IO (forall a. Sem (Delayed ': r) a -> Sem r a)
runDelayed lower = do
  ref <- newIORef ([] :: [A.Async ()])
  fixedNat $ \me ->
    interpretH \case
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
            let z = me $ m'
            postGUIASync $ void $ lower $ me $ m'
            modifyIORef ref $ drop 1
          modifyIORef ref $ (++ [a])
        getInitialStateT

