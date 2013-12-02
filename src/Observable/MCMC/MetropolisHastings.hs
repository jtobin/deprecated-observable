{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.MetropolisHastings (metropolisHastings) where

import Control.Lens
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V
import Observable.Core
import Observable.MCMC
import Statistics.Distribution
import Statistics.Distribution.Normal

isoGauss :: Vector Double -> Vector Double -> Double -> Double
isoGauss xs mu s = V.product $ V.zipWith density nds xs
  where nds = V.map (`normalDistr` s) mu

perturb :: PrimMonad m
        => Vector Double -> Double -> Observable m (Vector Double)
perturb q e = V.mapM (`normal` e) q

acceptRejectRatio
  :: Target Double -> Double -> Vector Double -> Vector Double -> Double
acceptRejectRatio target e current proposed = exp . min 0 $
    (target^.objective) proposed + log (isoGauss current proposed e)
  - (target^.objective) current  - log (isoGauss proposed current e)

nextState
  :: Double -> Target Double -> Double -> Vector Double -> Vector Double -> Vector Double
nextState z target e current proposed
    | z < acceptProb = proposed
    | otherwise      = current
  where
    arRatio    =  acceptRejectRatio target e current proposed
    acceptProb | isNaN arRatio = 0
               | otherwise     = arRatio

metropolisHastings :: Double -> TransitionOperator Double
metropolisHastings e t = do
  current  <- use parameterSpacePosition
  zc       <- lift $ unit
  proposed <- lift $ perturb current e
  let next  = nextState zc t e current proposed
  put $ Trace next (t^.objective $ next) e
  return next

