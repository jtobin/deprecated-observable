module Observable.MCMC.NUTS (nuts) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Observable.Types

import Debug.Trace

type Parameters = Vector Double
type Gradient   = Parameters -> Parameters
type Particle   = (Parameters, Parameters)

getStepSize :: Maybe Double -> OptionalStore -> Double
getStepSize (Just e) _    = e
getStepSize Nothing store = e where
  (ODouble e) = HashMap.lookupDefault (ODouble 0.1) NUTS store

updateStepSize :: Double -> OptionalStore -> OptionalStore
updateStepSize e = HashMap.insert NUTS (ODouble e) 

-- | The NUTS transition kernel.
nuts :: PrimMonad m => Transition m Double
nuts = do
    Chain t target _ store <- get
    r0          <- V.replicateM (V.length t) (lift $ normal 0 1)
    z0          <- lift $ exponential 1
    let logu     = log (auxilliaryTarget lTarget t r0) - z0
        lTarget  = logObjective target
        glTarget = handleGradient $ gradient target
        e        = getStepSize Nothing store

    let go (tn, tp, rn, rp, tm, j, n, s)
          | s == 1 = do
              vj <- lift $ categorical [-1, 1]
              z  <- lift unit

              (tnn, rnn, tpp, rpp, t1, n1, s1) <- 
                if   vj == -1
                then do
                  (tnn', rnn', _, _, t1', n1', s1') <- 
                    buildTree lTarget glTarget tn rn logu vj j e
                  return (tnn', rnn', tp, rp, t1', n1', s1')
                else do
                  (_, _, tpp', rpp', t1', n1', s1') <- 
                    buildTree lTarget glTarget tp rp logu vj j e
                  return (tn, rn, tpp', rpp', t1', n1', s1')

              let accept = s1 == 1 && (min 1 (fi n1 / fi n :: Double)) > z

                  n2 = n + n1
                  s2 = s1 * stopCriterion tnn tpp rnn rpp
                  j1 = succ j
                  t2 | accept    = t1
                     | otherwise = tm

              go (tnn, tpp, rnn, rpp, t2, j1, n2, s2)

          | otherwise = do
              put $ Chain tm target (lTarget tm) (updateStepSize e store)
              return tm

    go (t, t, r0, r0, t, 0, 1, 1)

buildTree lTarget glTarget t r logu v 0 e = do
  let (t0, r0) = leapfrog glTarget (t, r) (v * e)
      joint    = log $ auxilliaryTarget lTarget t0 r0
      n        = indicate (logu < joint)
      s        = indicate (logu - 1000 < joint)
  return (t0, r0, t0, r0, t0, n, s)

buildTree lTarget glTarget t r logu v j e = do
  z <- lift unit
  (tn, rn, tp, rp, t0, n0, s0) <- 
    buildTree lTarget glTarget t r logu v (pred j) e

  if   s0 == 1
  then do
    (tnn, rnn, tpp, rpp, t1, n1, s1) <- 
      if   v == -1
      then do
        (tnn', rnn', _, _, t1', n1', s1') <- 
          buildTree lTarget glTarget tn rn logu v (pred j) e
        return (tnn', rnn', tp, rp, t1', n1', s1')
      else do
        (_, _, tpp', rpp', t1', n1', s1') <- 
          buildTree lTarget glTarget tp rp logu v (pred j) e
        return (tn, rn, tpp', rpp', t1', n1', s1')

    let accept = (fi n1 / max (fi (n0 + n1)) 1) > (z :: Double)
        n2     = n0 + n1
        s2     = s0 * s1 * stopCriterion tnn tpp rnn rpp
        t2     | accept    = t1
               | otherwise = t0 

    return (tnn, rnn, tpp, rpp, t2, n2, s2)
  else return (tn, rn, tp, rp, t0, n0, s0)

-- | Determine whether or not to stop doubling the tree of candidate states.
stopCriterion :: (Integral a, Num b, Ord b, Unbox b)
  => Vector b -> Vector b -> Vector b -> Vector b -> a
stopCriterion tn tp rn rp = 
      indicate (positionDifference `innerProduct` rn >= 0)
    * indicate (positionDifference `innerProduct` rp >= 0)
  where
    positionDifference = tp .- tn

-- | Simulate a single step of Hamiltonian dynamics.
leapfrog :: Gradient -> Particle -> Double -> Particle
leapfrog glTarget (t, r) e = (tf, rf) where 
  rm = adjustMomentum glTarget e t r
  tf = adjustPosition e rm t
  rf = adjustMomentum glTarget e tf rm

-- | Adjust momentum.
adjustMomentum :: (Fractional c, Unbox c)
  => (t -> Vector c) -> c -> t -> Vector c -> Vector c
adjustMomentum glTarget e t r = r .+ ((e / 2) .* glTarget t)

-- | Adjust position.
adjustPosition :: (Num c, Unbox c) => c -> Vector c -> Vector c -> Vector c
adjustPosition e r t = t .+ (e .* r)

-- | The MH acceptance ratio for a given proposal.
acceptanceRatio :: (Floating a, Unbox a)
  => (t -> a) -> t -> t -> Vector a -> Vector a -> a
acceptanceRatio lTarget t0 t1 r0 r1 = auxilliaryTarget lTarget t1 r1
                                    / auxilliaryTarget lTarget t0 r0

-- | The negative potential. 
auxilliaryTarget :: (Floating a, Unbox a) => (t -> a) -> t -> Vector a -> a
auxilliaryTarget lTarget t r = exp (lTarget t - 0.5 * innerProduct r r)

-- | Simple inner product.
innerProduct :: (Num a, Unbox a) => Vector a -> Vector a -> a
innerProduct xs ys = V.sum $ V.zipWith (*) xs ys

-- | Vectorized multiplication.
(.*) :: (Num a, Unbox a) => a -> Vector a -> Vector a
z .* xs = V.map (* z) xs

-- | Vectorized subtraction.
(.-) :: (Num a, Unbox a) => Vector a -> Vector a -> Vector a
xs .- ys = V.zipWith (-) xs ys

-- | Vectorized addition.
(.+) :: (Num a, Unbox a) => Vector a -> Vector a -> Vector a
xs .+ ys = V.zipWith (+) xs ys

-- | Indicator function.
indicate :: Integral a => Bool -> a
indicate True  = 1
indicate False = 0

-- | Alias for fromIntegral.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

