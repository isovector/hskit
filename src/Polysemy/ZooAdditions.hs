{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}

module Polysemy.ZooAdditions where

import Polysemy
import Polysemy.IdempotentLowering

fixedNat
    :: forall f g m
     . Applicative m
    => ((forall x. f x -> g x) -> (forall x. f x -> g x))
    -> m (forall x. f x -> g x)
fixedNat f =
  let x :: (forall x. f x -> g x)
      x = f x
   in nat x

