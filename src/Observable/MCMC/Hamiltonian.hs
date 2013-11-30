{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.Hamiltonian where

-- messed something up somewhere..

import Control.Lens
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans
import Observable.Core
import Observable.MCMC

-- | Simulate a single step of Hamiltonian dynamics.
leapfrog :: Target Double -> [Double] -> [Double] -> Double -> ([Double], [Double])
leapfrog target t r e = (tf, rf) where 
  rm       = adjustMomentum glTarget e t r
  tf       = adjustPosition e rm t
  rf       = adjustMomentum glTarget e tf rm
  glTarget = handleGradient $ target^.gradient

leapfrogIntegrator target t0 r0 e = go t0 r0 where
  go t r 0 = (t, r)
  go t r n = let (t1, r1) = leapfrog target t r e
             in  go t1 r1 (pred n)

-- | Adjust momentum.
adjustMomentum :: Fractional c => (t -> [c]) -> c -> t -> [c] -> [c]
adjustMomentum glTarget e t r = r .+ ((e / 2) .* glTarget t)

-- | Adjust position.
adjustPosition :: Num c => c -> [c] -> [c] -> [c]
adjustPosition e r t = t .+ (e .* r)

-- | The MH acceptance ratio for a given proposal.
acceptanceRatio :: Floating a => (t -> a) -> t -> t -> [a] -> [a] -> a
acceptanceRatio lTarget t0 t1 r0 r1 = auxilliaryTarget lTarget t1 r1
                                    / auxilliaryTarget lTarget t0 r0

-- | The negative potential. 
auxilliaryTarget :: Floating a => (t -> a) -> t -> [a] -> a
auxilliaryTarget lTarget t r = exp (lTarget t - 0.5 * innerProduct r r)

-- | Simple inner product.
innerProduct :: Num a => [a] -> [a] -> a
innerProduct xs ys = sum $ zipWith (*) xs ys

-- | Vectorized multiplication.
(.*) :: Num b => b -> [b] -> [b]
z .* xs = map (* z) xs

-- | Vectorized subtraction.
(.-) :: Num a => [a] -> [a] -> [a]
xs .- ys = zipWith (-) xs ys

-- | Vectorized addition.
(.+) :: Num a => [a] -> [a] -> [a]
xs .+ ys = zipWith (+) xs ys

nextState z target q0 q1 r0 r1
    | z < min 0 arRatio = q1
    | otherwise         = q0
  where
    arRatio = acceptanceRatio (target^.objective) q0 q1 r0 r1

hamiltonianTransition :: Double -> TransitionOperator Double Double
hamiltonianTransition e target = do
  q0         <- use parameterSpacePosition 
  r0         <- replicateM (length q0) (lift $ standardNormal)
  zc         <- lift $ unit
  let (q, r) =  leapfrogIntegrator target q0 r0 e 20
      next   =  nextState zc target q0 q r0 r
  put $ Trace next (target^.objective $ next) e
  return next

