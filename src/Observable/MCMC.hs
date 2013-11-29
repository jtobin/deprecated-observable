{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Observable.MCMC where

import Control.Lens
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Observable.Core
import Statistics.Distribution
import Statistics.Distribution.Normal

-- testing
import Pipes
import System.Random.MWC hiding (uniform)

data Target a = Target {
    _objective :: [a] -> Double
  , _gradient  :: Maybe ([a] -> [a])
  }

data Trace a e = Trace {
    _parameterSpacePosition :: [a]
  , _dataSpacePosition      :: Double
  , _optionalInformation    :: e
  }

instance Show a => Show (Trace a e) where
  show = show . _parameterSpacePosition

type TransitionOperator a e =
  forall m. PrimMonad m => Target a -> StateT (Trace a e) (Observable m) [a]

makeLenses ''Trace
makeLenses ''Target

createTargetWithGradient :: ([a] -> Double) -> ([a] -> [a]) -> Target a
createTargetWithGradient f g  = Target f (Just g)

createTargetWithoutGradient :: ([a] -> Double) -> Target a
createTargetWithoutGradient f = Target f Nothing

initializeTrace :: Target a -> [a] -> e -> Trace a e
initializeTrace t as = Trace as (t^.objective $ as)

-- | Sample from some distribution indirectly via MCMC.
observeIndirectly :: Monad m => t -> (t -> StateT s m a) -> s -> Int -> m s
observeIndirectly f t o n = replicateM_ n (t f) `execStateT` o

-- | Better infix syntax for observeIndirectly.
observedIndirectlyBy :: Monad m => t -> (t -> StateT s m a) -> s -> Int -> m s
observedIndirectlyBy = observeIndirectly

-- | Transition operator composition.
interleave :: Monad m => (t -> m a) -> (t -> m b) -> t -> m b
interleave s0 s1 t = s0 t >> s1 t


-- testing

isoGauss xs mu s = product $ zipWith density nds xs
  where nds = map (`normalDistr` s) mu

perturb q e = mapM (`normal` e) q

metropolisTransition
  :: Double
  -> TransitionOperator Double Double
metropolisTransition e t = do
  Trace q _ _ <- get
  zc          <- lift $ uniform (0, 1)
  proposal    <- lift $ perturb q e

  let next | zc < acceptProb = proposal
           | otherwise       = q

      acceptProb | isNaN arRatio = 0
                 | otherwise     = arRatio

      arRatio = exp . min 0 $
          (t^.objective) proposal + log (isoGauss q proposal e)
        - (t^.objective) q - log (isoGauss proposal q e)  

  put $ Trace next (t^.objective $ next) e
  return next

compositeMetropolis :: TransitionOperator Double Double
compositeMetropolis = metropolisTransition 1.0 
         `interleave` metropolisTransition 0.5 
         `interleave` metropolisTransition 0.1 

-- | log-Rosenbrock function.
lRosenbrock :: RealFloat a => [a] -> a
lRosenbrock [x0, x1] = (-1) * (5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

-- | Gradient of log-Rosenbrock.
glRosenbrock :: RealFloat a => [a] -> [a]
glRosenbrock [x, y] =
  let dx = 20 * x * (y - x ^ 2) + 0.1 * (1 - x)
      dy = -10 * (y - x ^ 2)
  in  [dx, dy]

logRosenbrock :: Target Double
logRosenbrock = createTargetWithGradient lRosenbrock glRosenbrock

-- you ideally want to get rid of the 'trace' at this point.. hmmm
logRosenbrockVariate
  :: PrimMonad m => Trace Double Double -> Int -> Observable m (Trace Double Double)
logRosenbrockVariate = logRosenbrock `observedIndirectlyBy` compositeMetropolis

q0 = Trace [0.0, 0.0] (lRosenbrock [0.0, 0.0]) 0.5

main = do
  g <- create
  runEffect $ observe (logRosenbrockVariate q0 100) g >-> printer

