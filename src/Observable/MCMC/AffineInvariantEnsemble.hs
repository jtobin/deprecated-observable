{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.AffineInvariantEnsemble where

import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Statistics.Distribution
import Statistics.Distribution.Normal

type Ensemble = Vector [Double]

-- | Generate a random value from a distribution having the property that 
--   g(1/z) = z g(z).
symmetricVariate :: PrimMonad m => Observable m Double
symmetricVariate = do
  z <- unit
  return $ 0.5 * (z + 1) ^ 2

perturbParticle w0 w1 = do
  zs <- replicateM (length w0) symmetricVariate

V.zipWith (+) (V.map (* z) w0) (V.map (* (1 - z)) w1)


-- acceptanceRatio
--   :: [Double] -> [Double]
--   -> Double -> Double
--   -> ([Double] -> Double)
--   -> ([Double], Int)
-- acceptanceRatio target w0 w1 z zc = 
--   let val      = target proposal - target w0 + (fromIntegral (length w0) - 1) * log z
--       proposal = zipWith (+) (map (*z) w0) (map (*(1-z)) w1) 
--   in  if zc <= min 1 (exp val) then (proposal, 1) else (w0, 0)
-- 
-- -- | Calculate the acceptance ratio for a proposed move.
-- acceptRejectRatio
--   :: Target Double -> Double -> Vector Double -> Vector Double -> Double
-- acceptRejectRatio target e current proposed = exp . min 0 $
--     logObjective target proposed + log (isoGauss current proposed e)
--   - logObjective target current  - log (isoGauss proposed current e)


