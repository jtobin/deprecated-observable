{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.Anneal where -- (anneal, annealTransitions) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Observable.Types

annealTransitions
  :: PrimMonad m
  => [Transition m Double]
  -> MarkovChain m Double
annealTransitions ts = sequence . zipWith anneal annealingSchedule $ ts where
  annealingSchedule = schedule (length ts)

anneal :: PrimMonad m => Double -> Transition m Double -> Transition m Double
anneal invTemp baseTransition
  | invTemp < 0 = error "anneal: invalid temperture"
  | otherwise = do
      originalTarget <- gets objectiveFunction
      let annealedTarget = annealer invTemp originalTarget
      modify $ useTarget annealedTarget
      baseTransition
      modify $ useTarget originalTarget
      gets parameterSpacePosition

annealer :: Double -> Target Double -> Target Double
annealer invTemp target = Target annealedL annealedG where
  annealedL xs = invTemp * logObjective target xs
  annealedG    =
    case gradient target of
      Nothing -> Nothing
      Just g  -> Just (V.map (* invTemp) . g)

useTarget :: Target a -> Chain a -> Chain a
useTarget newTarget (Chain current _ _ store) =
  Chain current newTarget (logObjective newTarget current) store

schedule :: (Enum a, Floating a) => Int -> [a]
schedule n = initial ++ replicate d 1.0 where
  m = floor $ fromIntegral n / 5
  d = n - m
  initial = map (/ fromIntegral m) (logspace m 1 (fromIntegral m))

linspace :: (Enum a, Fractional a) => Int -> a -> a -> [a]
linspace n a b = map ((+) a . scale) [0..fromIntegral n - 1] where
  scale = (*) ((b - a) / fromIntegral (n - 1))

logspace :: (Enum a, Floating a) => Int -> a -> a -> [a]
logspace n a b = map (exp . (*) (log 10)) lins where
  lins = linspace n (logBase 10 a) (logBase 10 b)

