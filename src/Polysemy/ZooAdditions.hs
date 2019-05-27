{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}

module Polysemy.ZooAdditions where

import Polysemy
import Polysemy.IdempotentLowering


fixedNat
    :: forall m n base
     . Applicative base
    => ((forall x. m x -> n x) -> (forall x. m x -> n x))
    -> base (forall x. m x -> n x)
fixedNat f =
  let x :: (forall x. m x -> n x)
      x = f x
   in nat x


fixedNat'
    :: forall m n f base
     . Applicative base
    => ((forall x. m x -> n (f x)) -> (forall x. m x -> n (f x)))
    -> base (forall x. m x -> n (f x))
fixedNat' f =
  let x :: (forall x. m x -> n (f x))
      x = f x
   in nat' x

