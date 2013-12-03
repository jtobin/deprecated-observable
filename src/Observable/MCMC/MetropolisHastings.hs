{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.MetropolisHastings (metropolisHastings) where

import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Statistics.Distribution
import Statistics.Distribution.Normal

-- | Can this be written to more sanely handle unboxed vectors?
isoGauss :: Vector Double -> Vector Double -> Double -> Double
isoGauss xs mu s = V.product . V.fromList $ zipWith density nds (V.toList xs)
  where nds = map (`normalDistr` s) (V.toList mu)

-- | Perturb the state by kicking it with a Gaussian variate.
perturb
  :: PrimMonad m
  => Vector Double
  -> Double
  -> Observable m (Vector Double)
perturb q e = V.mapM (`normal` e) q

-- | Calculae the acceptance ratio for a proposed move.
acceptRejectRatio
  :: Target Double -> Double -> Vector Double -> Vector Double -> Double
acceptRejectRatio target e current proposed = exp . min 0 $
    logObjective target proposed + log (isoGauss current proposed e)
  - logObjective target current  - log (isoGauss proposed current e)

-- | Determine the next state of the chain.
nextState
  :: Double
  -> Target Double
  -> Double
  -> Vector Double
  -> Vector Double
  -> Vector Double
nextState z target e current proposed
    | z < acceptProb = proposed
    | otherwise      = current
  where
    arRatio    =  acceptRejectRatio target e current proposed
    acceptProb | isNaN arRatio = 0
               | otherwise     = arRatio

-- | The Metropolis-Hastings transition operator.
metropolisHastings :: Double -> Transition Double
metropolisHastings e t = do
  MarkovChain current _ _ <- get
  zc       <- lift unit
  proposed <- lift $ perturb current e
  let next  = nextState zc t e current proposed
  put $ MarkovChain next (logObjective t next) e
  return next

