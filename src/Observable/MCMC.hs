{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Observable.MCMC where

-- import Observable.MCMC.Hamiltonian
-- import Observable.MCMC.NUTS
-- import Observable.MCMC.Slice
import Control.Monad.Primitive
import Observable.Core
import Observable.MCMC.MetropolisHastings
import Observable.Types
import Control.Monad.Trans

interleave
  :: PrimMonad m
  => Transition m a -> Transition m a -> Transition m a
interleave t0 t1 = t0 >> t1

randomlyInterleave
  :: PrimMonad m
  => Transition m a -> Transition m a -> Transition m a
randomlyInterleave t0 t1 = do
  z <- lift unit
  if z then t0 else t1
  
biasedRandomlyInterleave
  :: PrimMonad m
  => Double
  -> Transition m a
  -> Transition m a
  -> Transition m a
biasedRandomlyInterleave p t0 t1 = do
  s <- lift $ bernoulli p
  if s then t0 else t1

interleaveMany :: PrimMonad m => [Transition m a] -> Transition m a
interleaveMany = foldl1 (>>) 

