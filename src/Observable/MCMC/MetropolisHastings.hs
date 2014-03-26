{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.MetropolisHastings where -- (metropolisHastings) where

import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Observable.Types
import Statistics.Distribution
import Statistics.Distribution.Normal

-- | Isometric Gaussian with mean mu standard deviation s.
isoGauss :: Vector Double -> Vector Double -> Double -> Double
isoGauss xs mu s = product $ zipWith density normalDists xsAsList where
  xsAsList    = V.toList xs
  muAsList    = V.toList mu
  normalDists = map (`normalDistr` s) muAsList

-- | Perturb something by kicking it with a Gaussian variate.
perturb
  :: PrimMonad m
  => Vector Double
  -> Double
  -> Observable m (Vector Double)
perturb q stepSize = V.mapM (`normal` stepSize) q

-- | Calculate the acceptance ratio for a proposed move.
acceptRatio
  :: Target Double -> Vector Double -> Vector Double -> Double -> Double
acceptRatio target current proposed stepSize = exp . min 0 $
    logObjective target proposed + log (isoGauss current proposed stepSize)
  - logObjective target current  - log (isoGauss proposed current stepSize)

-- | Determine the next state of the chain.
nextState
  :: Target Double
  -> Vector Double
  -> Vector Double
  -> Double
  -> Double
  -> Vector Double
nextState target current proposed stepSize z
    | z < acceptProb = proposed
    | otherwise      = current
  where
    ratio = acceptRatio target current proposed stepSize 
    acceptProb | isNaN ratio = 0
               | otherwise   = ratio

-- | Get the step size from the optional store.
getStepSize :: Maybe Double -> OptionalStore -> Double
getStepSize Nothing store = stepSize
  where (ODouble stepSize) = HashMap.lookupDefault (ODouble 1.0) MH store

getStepSize (Just stepSize) _ = stepSize

updateStepSize :: Double -> OptionalStore -> OptionalStore
updateStepSize stepSize store = HashMap.insert MH (ODouble stepSize) store

-- | The Metropolis-Hastings transition operator.
metropolisHastings :: PrimMonad m => Maybe Double -> Transition m Double
metropolisHastings e = do
  Chain current target _ store <- get
  let stepSize = getStepSize e store
  zc       <- lift unit
  proposed <- lift $ perturb current stepSize
  let next     = nextState target current proposed stepSize zc
      newStore = updateStepSize stepSize store
  put $ Chain next target (logObjective target next) newStore
  return next

