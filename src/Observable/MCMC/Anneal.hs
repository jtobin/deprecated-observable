{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.Anneal (anneal) where

import Control.Monad.Primitive
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Observable.Types

-- experimental
-- this needs to be handled differently, maybe by way of the store..

anneal
  :: PrimMonad m => Double -> Transition m Double -> Transition m Double
anneal s t = annealer s >> t >> annealer (1 / s)

annealer :: PrimMonad m => Double -> Transition m Double
annealer v = do
  Chain next (Target l mg) _ store <- get
  let l' xs = v * l xs
      g' = case mg of
             Nothing -> Nothing
             Just g  -> Just (V.map (* v) . g)
      target' = Target l' g'
  put $ Chain next target' (l' next) store
  return next

