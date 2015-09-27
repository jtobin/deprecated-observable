
module Observable.MCMC.MALA where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Observable.Core
import Observable.Types
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Statistics.Distribution
import Statistics.Distribution.Normal

sphereGauss :: Vector Double -> Vector Double -> Double -> Double
sphereGauss xs mu sd = product $ zipWith density normalDists xsAsList where
  xsAsList = V.toList xs
  muAsList = V.toList mu
  normalDists = map (`normalDistr` sd) muAsList

(.*) :: Double -> Vector Double -> Vector Double
z .* xs = V.map (* z) xs

(.+) :: Vector Double -> Vector Double -> Vector Double
xs .+ ys = V.zipWith (+) xs ys

localMean :: Target Double -> Vector Double -> Double -> Vector Double
localMean target position e = position .+ scaledGradient position where
  grad = handleGradient (gradient target)
  scaledGradient p = (0.5 * e * e) .* grad p

perturb
  :: PrimMonad m
  => Target Double
  -> Vector Double
  -> Double
  -> Observable m (Vector Double)
perturb target position e = do
  zs <- V.replicateM (V.length position) standardNormal
  return $ localMean target position e .+ (e .* zs)

mala :: PrimMonad m => Maybe Double -> Transition m Double
mala e = do
  Chain current target _ store <- get
  let step = getStepSize e store
  proposal <- lift $ perturb target current step
  zc       <- lift unit

  let cMean    = localMean target current step
      pMean    = localMean target proposal step
      next     = nextState target (current, cMean) (proposal, pMean) step zc
      newStore = updateStepSize step store

  put $ Chain next target (logObjective target next) newStore
  return next

getStepSize :: Maybe Double -> OptionalStore -> Double
getStepSize (Just step) _ = step
getStepSize Nothing store = step where
  (ODouble step) = HashMap.lookupDefault (ODouble 1.0) MALA store

updateStepSize :: Double -> OptionalStore -> OptionalStore
updateStepSize step = HashMap.insert MALA (ODouble step)

nextState
  :: Target Double
  -> (Vector Double, Vector Double)
  -> (Vector Double, Vector Double)
  -> Double
  -> Double
  -> Vector Double
nextState target (current, cMean) (proposal, pMean) e z
    | z < acceptProb = proposal
    | otherwise      = current
  where
    ratio = acceptRatio target (current, cMean) (proposal, pMean) e
    acceptProb | isNaN ratio = 0
               | otherwise   = ratio

acceptRatio
  :: Target Double
  -> (Vector Double, Vector Double)
  -> (Vector Double, Vector Double)
  -> Double
  -> Double
acceptRatio target (current, cMean) (proposal, pMean) e = exp . min 0 $
    logObjective target proposal + log (sphereGauss current pMean e)
  - logObjective target current  - log (sphereGauss proposal cMean e)

