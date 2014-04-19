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

frequency :: PrimMonad m => [(Int, Transition m a)] -> Transition m a
frequency xs = lift (uniform (1, tot)) >>= (`pick` xs) where
  tot = sum . map fst $ xs
  pick n ((k, v):vs)
    | n <= k = v
    | otherwise = pick (n - k) vs

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
traceChain n t o = observe $ replicateM n t `evalStateT` o

