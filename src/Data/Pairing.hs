{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Pairing (Pairing(..)) where

import Protolude
import qualified Control.Monad.Free as Free
import qualified Control.Comonad.Cofree as Cofree

class (Functor f, Functor g) => Pairing f g where
  pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

instance Pairing f g => Pairing (Cofree.Cofree f) (Free.Free g) where
  pair p (a Cofree.:< _) (Free.Pure x) = p a x
  pair p (_ Cofree.:< fs) (Free.Free gs) = pair (pair p) fs gs
