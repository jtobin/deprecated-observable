{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Observable.MCMC where

import Control.Applicative
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

-- | Trace a Markov chain realized by repeating the same transition operator n
--   times.
traceChain
  :: Monad m
  => Int
  -> Transition m a
  -> Chain a
  -> Gen (PrimState m)
  -> m [Vector a]
traceChain n t o = observe $ replicateM n t `evalStateT` o


-- | Trace a Markov chain.
traceMarkovChain
  :: Monad m
  => MarkovChain m a
  -> Chain a
  -> Gen (PrimState m)
  -> m [Vector a]
traceMarkovChain c o = observe $ c `evalStateT` o

-- | Swap the positions of coupled chains.
-- swap :: Monad m => CoupledChains m a
-- swap = do
--   (Chain q0 t0 _ s0, Chain q1 t1 _ s1) <- get 
--   let (l0, l1) = (logObjective t0 q1, logObjective t1 q0)
--   put (Chain q1 t0 l0 s0, Chain q0 t1 l1 s1)
--   return q1
 
-- | Possibly swap positions of coupled chains with swap probability p.
--
--   could stand to fix this up a bit
-- proposeSwap :: PrimMonad m => Double -> CoupledChains m a
-- proposeSwap p = do
--   oldState <- get
--   b        <- lift $ bernoulli p
--   when b $ do
--     swap
--     z <- log <$> lift unit
--     (Chain q0 (Target f _)  _ s0, Chain q1 (Target g _) _ s1) <- get
--     unless (f q1 + g q0 - f q0 - g q1 > z) $
--       void swap
-- 
--   (Chain q _ _ _, _) <- get
--   return q


