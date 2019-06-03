{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Polysemy.Webkit where

import           Data.IORef
import qualified Data.Trie as T
import           Polysemy
import           Polysemy.Delayed
import           Polysemy.Navigation
import           Polysemy.Operators
import qualified Polysemy.State as S
import           Polysemy.State hiding (get)


esc :: Char
esc = '\65307'


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
        (Just t'', Just f) -> do
          delay 1000000 $ do
            raise $ raise f
            S.put t
          S.put t''
        (Just t'', Nothing) -> do
          cancel
          S.put t''
      pure True

