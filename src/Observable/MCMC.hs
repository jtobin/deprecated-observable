{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

-- The state components that you want to keep track of:
--   - parameter space position 
--   - range space position (and gradient?  do you really need to cache this?)
--   - additional state piece

module Observable.MCMC where

import Control.Arrow
import Control.Lens
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Observable.Core
import Statistics.Distribution
import Statistics.Distribution.Normal

-- | A Target consists of an objective function and possibly its gradient.
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

makeLenses ''Trace
makeLenses ''Target

createTargetWithGradient :: ([a] -> Double) -> ([a] -> [a]) -> Target a
createTargetWithGradient f g  = Target f (Just g)

createTargetWithoutGradient :: ([a] -> Double) -> Target a
createTargetWithoutGradient f = Target f Nothing

type TransitionOperator a m e = forall b. StateT (Trace a e) m b





isoGauss xs mu s = product $ zipWith density nds xs
  where nds = map (`normalDistr` s) mu

perturb q e = mapM (\m -> normal m e) q

metropolisTransition t e = do
  Trace q _ _ <- get
  zc          <- lift $ uniform (0, 1)
  proposal    <- lift $ perturb q e

  let next | zc < acceptProb = proposal
           | otherwise       = q

      acceptProb = if isNaN arRatio then 0 else arRatio

      arRatio = exp . min 0 $
          (t^.objective) proposal + log (isoGauss q proposal e)
        - (t^.objective) q - log (isoGauss proposal q e)  

  put $ Trace next (t^.objective $ next) e
  return next


testTransition t = metropolisTransition t 0.1 >> metropolisTransition t 1.0




-- testing

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

rTrace = Trace [0.0, 0.0] (lRosenbrock [0.0, 0.0]) 0.5

