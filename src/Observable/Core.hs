{-# LANGUAGE BangPatterns #-}

module Observable.Core where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Observable.Types
import System.Random.MWC hiding (uniform)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.Dist

observeIO :: Observable IO r -> IO r
observeIO = withSystemRandom . asGenIO . observe

expectation
  :: Monad m
  => Int
  -> (a -> Double)
  -> Observable m a
  -> Gen (PrimState m)
  -> m Double
expectation n f p = observe $ go n 0 where
  go 0  !s = return s
  go !j !s = do
    x <- liftM f p
    go (pred j) (s + x / fromIntegral n)

expectationIO
  :: Int
  -> (a -> Double)
  -> Observable IO a
  -> IO Double
expectationIO n f p = withSystemRandom . asGenIO $ expectation n f p

jointIndependent :: Monad m => m a -> m b -> m (a, b)
jointIndependent = liftM2 (,)

jointConditional :: Monad m => m a -> (a -> m b) -> m (a, b)
jointConditional f g = do
  x <- f
  y <- g x
  return (x, y)

unit :: (PrimMonad m, Variate a) => Observable m a
unit = Observable MWC.uniform

uniform :: (PrimMonad m, Variate a) => (a, a) -> Observable m a
uniform r = Observable $ MWC.uniformR r

categorical :: PrimMonad m => [a] -> Observable m a
categorical cs = do
  j <- uniform (0, length cs - 1)
  return $ cs !! j

standardNormal :: PrimMonad m => Observable m Double
standardNormal = Observable MWC.Dist.standard

normal :: PrimMonad m => Double -> Double -> Observable m Double
normal m sd = Observable $ MWC.Dist.normal m sd

logNormal :: PrimMonad m => Double -> Double -> Observable m Double
logNormal m sd = exp <$> normal m sd

exponential :: PrimMonad m => Double -> Observable m Double
exponential r = Observable $ MWC.Dist.exponential r

gamma :: PrimMonad m => Double -> Double -> Observable m Double
gamma a b = Observable $ MWC.Dist.gamma a b

invGamma :: PrimMonad m => Double -> Double -> Observable m Double
invGamma a b = recip <$> gamma a b

chiSquare :: PrimMonad m => Int -> Observable m Double
chiSquare k = Observable $ MWC.Dist.chiSquare k

beta :: PrimMonad m => Double -> Double -> Observable m Double
beta a b = do
  u <- gamma a 1
  w <- gamma b 1
  return $ u / (u + w)

dirichlet :: PrimMonad m => [Double] -> Observable m [Double]
dirichlet as = do
  zs <- mapM (`gamma` 1) as
  return $ map (/ sum zs) zs

bernoulli :: PrimMonad m => Double -> Observable m Bool
bernoulli p = (< p) <$> unit

binomial :: PrimMonad m => Int -> Double -> Observable m Int
binomial n p = liftM (length . filter id) $ replicateM n (bernoulli p)

-- -- | Abstract distributions with countable support.
-- --countableSupport
-- --  :: (Enum a, Num a, Num b, Ord b, Variate b, PrimMonad m)
-- --  => (a -> (b, Observable m c))
-- --  -> Observable m c
-- --countableSupport f = unit >>= scan 1 where
-- --  scan n r
-- --    | r - fst (f n) <= 0 = snd (f n)
-- --    | otherwise          = scan (succ n) (r - fst (f n))
-- 
-- -- | Example discrete distribution expressed in above fashion.
-- -- ex: observeIO $ countableSupport exampleDiscrete
-- --exampleDiscrete j = dist !! (j - 1)
-- --  where dist = [(0.01, return 1), (0.9, return 2), (0.09, return 3)]

