module Data.Vector.Extra
  ( mapMaybe
  ) where

import Data.Vector as V
import Data.Vector.Generic
import Data.Vector.Fusion.Stream.Monadic
import Data.Vector.Fusion.Bundle.Size (toMax)
import Data.Vector.Fusion.Bundle (inplace)

-- | /O(n)/ Drop elements when predicate returns Nothing
-- Backport from 0.12.0.0.
mapMaybe :: (a -> Maybe b) -> V.Vector a -> V.Vector b
{-# INLINE mapMaybe #-}
mapMaybe f = unstream . inplace (sMapMaybe f) toMax . stream

sMapMaybe :: Monad m => (a -> Maybe b) -> Stream m a -> Stream m b
sMapMaybe f (Stream step t) = Stream step' t
  where
    step' s = do
                r <- step s
                case r of
                  Yield x s' -> do
                                  return $ case f x of
                                    Nothing -> Skip s'
                                    Just b' -> Yield b' s'
                  Skip    s' -> return $ Skip s'
                  Done       -> return $ Done
