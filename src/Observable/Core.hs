{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Observable.Core where

import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import Pipes
import System.Random.MWC hiding (uniform)
import qualified System.Random.MWC as MWC
import qualified System.Random.MWC.Distributions as MWC.Dist

-- | The Observable monad transformer provides an environment for managing
--   uncertainty. 
newtype Observable m a = Observable { sample :: Gen (PrimState m) -> m a }

instance PrimMonad m => Functor (Observable m) where
  fmap h (Observable f) = Observable $ liftM h . f

instance PrimMonad m => Applicative (Observable m) where
  pure  = return
  (<*>) = ap

instance PrimMonad m => Monad (Observable m) where
  return x = Observable $ const (return x)
  m >>= h  = Observable $ \g -> do
    z <- sample m g
    sample (h z) g

instance MonadTrans Observable where
  lift m = Observable $ const m

-- | A Target is something that can be sampled from by MCMC.
data Target a = Target {
    logObjective :: Vector a -> Double
  , gradient     :: Maybe (Vector a -> Vector a)
  }

-- | The current state of a Markov chain.
data MarkovChain a = MarkovChain {
    parameterSpacePosition :: Vector a
  , dataSpacePosition      :: Double
  , optionalInformation    :: a
  }

instance (Show a, Unbox a) => Show (MarkovChain a) where
  show = show . parameterSpacePosition

-- | A transition operator for driving a Markov chain.
type Transition a = forall m. PrimMonad m =>
  Target a -> StateT (MarkovChain a) (Observable m) (Vector a)

-- | Stream observations from a distribution.
observe
  :: PrimMonad m
  => Observable m a
  -> Gen (PrimState m)
  -> Producer a m b
observe (Observable f) g = forever $ lift (f g) >>= yield

-- | Sample from a distribution in the IO monad.
sampleIO :: Observable IO r -> IO r
sampleIO = withSystemRandom . asGenIO . sample

-- | Sample from a distribution concurrently in the IO monad.
sampleConcurrently :: Int -> Observable IO b -> IO [b]
sampleConcurrently n (Observable f) = mapConcurrently h (replicate n ())
  where h _x = withSystemRandom . asGenIO $ \g -> f g

-- | Generic expectation operator.
expectationGeneric
  :: (Fractional a, Integral b, PrimMonad m)
  => b
  -> (c -> a)
  -> Observable m c
  -> Observable m a
expectationGeneric n f p = go n 0 where
  go !0 !s = return s
  go !j !s = do
    x <- liftM f p
    go (pred j) (s + x / fromIntegral n)

-- | Expectation query, with provided PRNG.
expectation
  :: (Fractional a, Integral b, PrimMonad m)
  => b
  -> (c -> a)
  -> Observable m c
  -> Gen (PrimState m)
  -> m a
expectation n f p = sample $ expectationGeneric n f p

-- | Expectation query in IO.
expectationIO
  :: (Fractional a, Integral b)
  => b
  -> (c -> a)
  -> Observable IO c
  -> IO a
expectationIO n f p = sampleIO $ expectationGeneric n f p

-- | Joint (independent) distribution.
joint :: Monad m => m a -> m b -> m (a, b)
joint = liftM2 (,)

-- | Joint (conditional) distribution.
jointConditional :: Monad m => m a -> (a -> m b) -> m (a, b)
jointConditional f g = do
  x <- f
  y <- g x
  return (x, y)
  
-- | Uniform over some type.
unit :: (PrimMonad m, Variate a) => Observable m a
unit = Observable MWC.uniform

-- | Uniform over some range
uniform :: (PrimMonad m, Variate a) => (a, a) -> Observable m a
uniform r = Observable $ MWC.uniformR r

-- | Uniform over categories.
categorical :: PrimMonad m => [a] -> Observable m a
categorical cs = do
  j <- uniform (0, length cs - 1)
  return $ cs !! j

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

-- | Dirichlet variate.
dirichlet :: PrimMonad m => [Double] -> Observable m [Double]
dirichlet as = do
  zs <- mapM (`gamma` 1) as
  return $ map (/ sum zs) zs

-- | Bernoulli draw.
bernoulli :: PrimMonad m => Double -> Observable m Bool
bernoulli p = (< p) <$> unit

-- | Binomial draw.
binomial :: PrimMonad m => Int -> Double -> Observable m Int
binomial n p = liftM (length . filter id) $ replicateM n (bernoulli p)

-- | Receive input from a stream and print it to stdout.
display :: Show a => Consumer a IO ()
display = forever $ await >>= lift . print

-- | Collect n results.
collect :: Monad m => Int -> Consumer a m [a]
collect n = replicateM n await

-- | Target constructor using a gradient.
createTargetWithGradient
  :: (Vector a -> Double) -> (Vector a -> Vector a) -> Target a
createTargetWithGradient f g  = Target f (Just g)

-- | Target constructor sans gradient.
createTargetWithoutGradient :: (Vector a -> Double) -> Target a
createTargetWithoutGradient f = Target f Nothing

-- | Markov chain constructor.
initializeChain :: Unbox a => Target a -> [a] -> a -> MarkovChain a
initializeChain t as = MarkovChain vs (logObjective t vs)
  where vs = V.fromList as

-- | Create an Observable by using MCMC.
withMcmc
  :: PrimMonad m
  => Transition a
  -> MarkovChain a
  -> Int
  -> Target a
  -> Observable m (MarkovChain a)
withMcmc t o n f = replicateM_ n (t f) `execStateT` o

-- | Return the trace of a Markov chain.
traceChain
  :: PrimMonad m
  => Transition a
  -> MarkovChain a
  -> Int
  -> Target a
  -> Gen (PrimState m)
  -> m [Vector a]
traceChain t o n f = sample $ replicateM n (t f) `evalStateT` o

-- | Transition operator composition.
interleave :: Transition a -> Transition a -> Transition a
interleave t0 t1 target = t0 target >> t1 target

-- | Random transition operator composition.
randomlyInterleave :: Transition a -> Transition a -> Transition a
randomlyInterleave t0 t1 target = do
  s <- lift $ categorical [t0, t1]
  s target

-- | Simple gradient error handling.
handleGradient :: Maybe t -> t
handleGradient Nothing  = error "handleGradient: no gradient provided"
handleGradient (Just g) = g

