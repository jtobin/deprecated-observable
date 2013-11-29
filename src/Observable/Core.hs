{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Observable.Core where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.Trans
import System.Random.MWC hiding (uniform)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.Dist
import Pipes

data Observable m a = Observable { sampler :: Gen (PrimState m) -> m a }

instance PrimMonad m => Functor (Observable m) where
  fmap h (Observable f) = Observable $ liftM h . f

instance PrimMonad m => Applicative (Observable m) where
  pure  = return
  (<*>) = ap

instance PrimMonad m => Monad (Observable m) where
  return x = Observable $ const (return x)
  m >>= h  = Observable $ \g -> do
    z <- sampler m g
    sampler (h z) g

instance MonadTrans Observable where
  lift m = Observable $ const m

-- | Stream observations from a distribution.
observe
  :: PrimMonad m
  => Observable m a
  -> Gen (PrimState m)
  -> Producer a m b
observe (Observable f) g = forever $ lift (f g) >>= yield

-- | Sample from a distribution
sample :: PrimMonad m => Observable m r -> Gen (PrimState m) -> m r
sample f g = runEffect $ observe f g >-> await

-- | Joint (product) distribution.
joint :: Monad m => m a -> m b -> m (a, b)
joint = liftM2 (,)

-- | Conditional distribution.
conditional :: Monad m => m a -> (a -> m b) -> m (a, b)
conditional f g = do
  x <- f
  y <- g x
  return (x, y)
  
-- | Uniform over some type.
unit :: (PrimMonad m, Variate a) => Observable m a
unit = Observable MWC.uniform

-- | Uniform over some range
uniform :: (PrimMonad m, Variate a) => (a, a) -> Observable m a
uniform r = Observable $ uniformR r

-- | Standard normal variate.
standardNormal :: PrimMonad m => Observable m Double
standardNormal = Observable MWC.Dist.standard

-- | Normal variate with given mean and standard deviation.
normal :: PrimMonad m => Double -> Double -> Observable m Double
normal m s = Observable $ MWC.Dist.normal m s

-- | Log-normal variate with given mean and standard deviation.
logNormal :: PrimMonad m => Double -> Double -> Observable m Double
logNormal m s = exp <$> normal m s

-- | Exponential variate.
exponential :: PrimMonad m => Double -> Observable m Double
exponential r = Observable $ MWC.Dist.exponential r

-- | Gamma variate.
gamma :: PrimMonad m => Double -> Double -> Observable m Double
gamma a b = Observable $ MWC.Dist.gamma a b

-- | Inverse-gamma variate.
invGamma :: PrimMonad m => Double -> Double -> Observable m Double
invGamma a b = recip <$> gamma a b

-- | Chi-square variate.
chiSquare :: PrimMonad m => Int -> Observable m Double
chiSquare k = Observable $ MWC.Dist.chiSquare k

-- | Beta variate.
beta :: PrimMonad m => Double -> Double -> Observable m Double
beta a b = do
  u <- gamma a 1
  w <- gamma b 1
  return $ u / (u + w)

-- | Bernoulli draw.
bernoulli :: PrimMonad m => Double -> Observable m Bool
bernoulli p = (< p) <$> unit

-- | Binomial draw.
binomial :: PrimMonad m => Int -> Double -> Observable m Int
binomial n p = liftM (length . filter id) $ replicateM n (bernoulli p)

-- | Receive input from a stream and print it to stdout.
printer :: Show a => Consumer a IO ()
printer = forever $ await >>= lift . print

-- | Collect n results.
collect :: Monad m => Int -> Consumer a m [a]
collect n = replicateM n await

-- Deprecate below to VC

-- | Standard normal variate.
-- standardNormal :: (Floating b, Variate b, PrimMonad m) => Observable m b
-- standardNormal = do
--   u0 <- unit
--   u1 <- unit
--   return $ sqrt (-2.0 * log u0) * cos (2.0 * pi * u1)

-- | Normal with given mean/variance.
-- normal :: (Floating b, Variate b, PrimMonad m) => b -> b -> Observable m b
-- normal m v = do
--   u <- standardNormal
--   return $ u * sqrt v + m

-- -- | Lognormal distribution.
-- logNormal :: Double -> Double -> Prob Double
-- logNormal m v = exp <$> normal m v
-- 
-- -- | Exponential distribution.
-- exponential :: Double -> Prob Double
-- exponential r = do
--   z <- unit
--   return . negate $ log (1 - z) / r

-- -- | Given a seed, return an infinite list of draws from the sampling function.
-- runProb :: Seed -> Prob a -> [a]
-- runProb seed0 s@(Sampler g) = x : runProb seed1 s
--   where (x, seed1) = g seed0
-- 
-- -- | Joint distribution for independent variates.
-- jointIndependent :: Prob a -> Prob b -> Prob (a, b)
-- jointIndependent = liftM2 (,)
-- 
-- -- | Joint distribution for dependent variates.
-- jointConditional :: Prob a -> (a -> Prob b) -> Prob (a, b)
-- jointConditional f g = do
--   x <- f
--   y <- g x
--   return (x, y)
-- 
-- -- | Uniform distribution on (0, 1].  
-- unit :: Prob Double
-- unit = Sampler randomDouble
-- 
-- -- | Uniform distribution on [x, y].
-- uniform :: Fractional a => a -> a -> Prob a
-- uniform a b = scaleBy <$> unit
--   where scaleBy x = realToFrac x * (b - a) + a
-- 
-- -- | Generate a N(0, 1) variate.
-- unormal :: Prob Double
-- unormal = do
--   u0 <- unit
--   u1 <- unit
--   return $ sqrt (negate 2 * log u0) * cos (2 * pi * u1)
-- 
-- -- | Generate a N(m, v) variate.
-- normal :: Double -> Double -> Prob Double
-- normal m v = do
--   u <- unormal
--   return $ u * sqrt v + m
-- 
-- -- | Lognormal distribution.
-- logNormal :: Double -> Double -> Prob Double
-- logNormal m v = exp <$> normal m v
-- 
-- -- | Exponential distribution.
-- exponential :: Double -> Prob Double
-- exponential r = do
--   z <- unit
--   return . negate $ log (1 - z) / r
-- 
-- -- | Bernoulli distribution.
-- bernoulli :: Double -> Prob Bool
-- bernoulli p = (< p) <$> unit
-- 
-- -- | Binomial distribution.
-- binomial :: Int -> Double -> Prob Int
-- binomial n p = liftM countTrue $ replicateM n (bernoulli p)
-- 
-- -- | Estimate the probability that a hypothesis is true.
-- eval :: Int -> Prob Bool -> Prob Double
-- eval n s = do
--   bs <- replicateM n s
--   return $ realToFrac (countTrue bs) / fromIntegral n
