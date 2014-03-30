{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.MetropolisHastings (metropolisHastings) where

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

sphericalGaussian :: Vector Double -> Vector Double -> Double -> Double
sphericalGaussian xs mu sd = product $ zipWith density normalDists xsAsList
  where
    xsAsList    = V.toList xs
    muAsList    = V.toList mu
    normalDists = map (`normalDistr` sd) muAsList

perturb
  :: PrimMonad m
  => Vector Double
  -> Double
  -> Observable m (Vector Double)
perturb q sd = V.mapM (`normal` sd) q

acceptRatio
  :: Target Double -> Vector Double -> Vector Double -> Double -> Double
acceptRatio target current proposed sd = exp . min 0 $
    logObjective target proposed + log (sphericalGaussian current proposed sd)
  - logObjective target current  - log (sphericalGaussian proposed current sd)

nextState
  :: Target Double
  -> Vector Double
  -> Vector Double
  -> Double
  -> Double
  -> Vector Double
nextState target current proposed sd z
    | z < acceptProb = proposed
    | otherwise      = current
  where
    ratio = acceptRatio target current proposed sd 
    acceptProb | isNaN ratio = 0
               | otherwise   = ratio

getStandardDeviation :: Maybe Double -> OptionalStore -> Double
getStandardDeviation (Just sd) _   = sd
getStandardDeviation Nothing store = sd where
  (ODouble sd) = HashMap.lookupDefault (ODouble 1.0) MH store

updateStandardDeviation :: Double -> OptionalStore -> OptionalStore
updateStandardDeviation sd = HashMap.insert MH (ODouble sd) 

metropolisHastings :: PrimMonad m => Maybe Double -> Transition m Double
metropolisHastings e = do
  Chain current target _ store <- get
  let sd = getStandardDeviation e store
  proposed <- lift $ perturb current sd
  zc       <- lift unit
  let next     = nextState target current proposed sd zc
      newStore = updateStandardDeviation sd store
  put $ Chain next target (logObjective target next) newStore
  return next

