{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.MetropolisHastings (metropolisTransition) where

import Control.Lens
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Observable.Core
import Observable.MCMC
import Statistics.Distribution
import Statistics.Distribution.Normal

isoGauss ::  [Double] -> [Double] -> Double -> Double
isoGauss xs mu s = product $ zipWith density nds xs
  where nds = map (`normalDistr` s) mu

perturb :: PrimMonad m => [Double] -> Double -> Observable m [Double]
perturb q e = mapM (`normal` e) q

acceptRejectRatio :: Target Double -> Double -> [Double] -> [Double] -> Double
acceptRejectRatio target e current proposed = exp . min 0 $
    (target^.objective) proposed + log (isoGauss current proposed e)
  - (target^.objective) current  - log (isoGauss proposed current e)

nextState
  :: Double -> Target Double -> Double -> [Double] -> [Double] -> [Double]
nextState z target e current proposed
    | z < acceptProb = proposed
    | otherwise      = current
  where
    arRatio    =  acceptRejectRatio target e current proposed
    acceptProb | isNaN arRatio = 0
               | otherwise     = arRatio

metropolisTransition :: Double -> TransitionOperator Double
metropolisTransition e t = do
  current  <- use parameterSpacePosition
  zc       <- lift $ unit
  proposed <- lift $ perturb current e
  let next  = nextState zc t e current proposed
  put $ Trace next (t^.objective $ next) e
  return next

