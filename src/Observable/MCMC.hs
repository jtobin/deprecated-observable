{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Observable.MCMC where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.Vector.Unboxed (Vector)
import Observable.Core
import Observable.Types
import System.Random.MWC (Gen)

interleave :: Monad m => [Transition m a] -> Transition m a
interleave = foldl1 (>>) 

oneOf :: PrimMonad m => [Transition m a] -> Transition m a
oneOf ts = do
  j <- lift $ uniform (0, length ts - 1)
  ts !! j

firstWithProb
  :: PrimMonad m 
  => Double
  -> Transition m a
  -> Transition m a
  -> Transition m a
firstWithProb p t0 t1 = do
  s <- lift $ bernoulli p
  if s then t0 else t1

traceChain
  :: Monad m
  => Int
  -> Transition m a
  -> Chain a
  -> Gen (PrimState m)
  -> m [Vector a]
traceChain n t o = sample $ replicateM n t `evalStateT` o

