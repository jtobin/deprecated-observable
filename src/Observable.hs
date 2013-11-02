{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Control.Applicative
import Control.Arrow
import Control.Error
import Control.Monad
import Data.List
import Numeric.LinearAlgebra hiding (find)
import System.Environment
import System.Random.Mersenne.Pure64

type Seed = PureMT

data Prob a = 
    Sampler { observe :: Seed -> (a, Seed) }
  | Samples [a]

instance Functor Prob where
  fmap f (Sampler g)  = Sampler $ first f . g
  fmap f (Samples xs) = Samples $ fmap f xs

instance Applicative Prob where
  pure  = return
  (<*>) = ap

instance Monad Prob where
  return x          = Sampler $ (,) x

  (Sampler g) >>= f = Sampler $ \seed0 ->
    let (x, seed1) = g seed0
    in case f x of 
      Sampler h  -> h seed1
      Samples xs -> oneOfPrimitive xs seed1

  (Samples xs) >>= f = Sampler $ \seed0 ->
    let (x, seed1) = oneOfPrimitive xs seed0
    in case f x of
      Sampler g  -> g seed1
      Samples ys -> oneOfPrimitive ys seed1

-- | Given a seed, return an infinite list of draws from the sampling function.
runProb :: Seed -> Prob a -> [a]
runProb _     (Samples xs)  = xs
runProb seed0 s@(Sampler g) = x : runProb seed1 s
  where (x, seed1) = g seed0

-- | Return a list of draws from some sampling function in IO.
runProbIO :: Prob a -> IO [a]
runProbIO s = (`runProb` s) <$> getSeedIO

-- | Get a seed from the environment or generate a new one using the system 
--   clock.
getSeedIO :: IO Seed
getSeedIO = do
  args <- getArgs 
  case mapMaybe (stripPrefix "--seed=") args of
    []      -> newPureMT
    sdStr:_ -> return $ pureMT $ read sdStr

-- | Return a single draw from a sampling function.
sampleIO :: Prob a -> IO a
sampleIO = fmap head . runProbIO

-- | Return a list of n draws from a sampling function.
sampleNIO :: Int -> Prob a -> IO [a]
sampleNIO n s = take n <$> runProbIO s

-- | Estimate the probability that a hypothesis is true.
eval :: Int -> Prob Bool -> Prob Double
eval n s = do
  bs <- replicateM n s
  return $ realToFrac (countTrue bs) / fromIntegral n

-- | Joint distribution for independent variates.
joint :: Prob a -> Prob b -> Prob (a, b)
joint = liftM2 (,)

-- | Joint distribution for dependent variates.
jointConditional :: Prob a -> (a -> Prob b) -> Prob (a, b)
jointConditional f g = do
  x <- f
  y <- g x
  return (x, y)

-- | Uniform distribution on (0, 1].
unit :: Prob Double
unit = Sampler randomDouble

-- | Uniform distribution on [x, y].
uniform :: Fractional a => a -> a -> Prob a
uniform a b = scaleBy <$> unit
  where scaleBy x = realToFrac x * (b - a) + a

-- | Generate a N(0, 1) variate.
unormal :: Prob Double
unormal = do
  u0 <- unit
  u1 <- unit
  return $ sqrt (negate 2 * log u0) * cos (2 * pi * u1)

-- | Generate a N(m, v) variate.
normal :: Double -> Double -> Prob Double
normal m v = do
  u <- unormal
  return $ u * sqrt v + m

-- | Generate a bunch of N(m, v) variates.
normalMany :: [(Double, Double)] -> Prob [Double]
normalMany mvs = do
  gus <- normalManyUnit (length mvs)
  let f (gu, (m, v)) = gu * sqrt v + m
  return $ map f (zip gus mvs)

-- | Generate a bunch of N(0, 1) variates.
normalManyUnit :: Int -> Prob [Double]
normalManyUnit 0 = return []
normalManyUnit n
  | odd n     = liftM2 (:) unormal (normalManyUnit (n - 1))
  | otherwise = do
      us <- forM [1..n] $ const $ unit
      return $ gaussTwoAtATime $ map realToFrac us

-- | Generate two N(0, 1) variates at a time.
gaussTwoAtATime :: Floating a => [a] -> [a]
gaussTwoAtATime (u0:u1:rest) = 
    sqrt (negate 2 * log u0) * cos (2 * pi * u1)
  : sqrt (negate 2 * log u0) * sin (2 * pi * u1)
  : gaussTwoAtATime rest
gaussTwoAtATime _ = []

-- | Multivariate Gaussian.
multiNormal :: Vector Double -> Matrix Double -> Prob (Vector Double)
multiNormal mu sigma = do
  let c = cholSH sigma
      a = trans c
      k = dim mu

  z <- fromList <$> normalManyUnit k

  return $ mu + (head . toColumns $ a `multiply` asColumn z)

-- | Lognormal distribution.
logNormal :: Double -> Double -> Prob Double
logNormal m v = exp <$> normal m v

-- | Bernoulli distribution.
bernoulli :: Double -> Prob Bool
bernoulli p = (< p) <$> unit

-- | Exponential distribution.
expDist :: Double -> Prob Double
expDist r = do
  z <- unit
  return . negate $ log (1 - z) / r

-- | Binomial distribution.
binomial :: Int -> Double -> Prob Int
binomial n p = replicateM n (bernoulli p) >>= return . countTrue

-- | Count the number of Trues in a list.
countTrue :: [Bool] -> Int
countTrue = length . filter id

-- | Select a random element from a list. 
oneOfPrimitive :: [a] -> Seed -> (a, Seed)
oneOfPrimitive xs seed =
  let (u, nextSeed) = randomDouble seed
      idx           = u `timesLengthOf` xs
  in  (xs !! idx, nextSeed)

-- | Multiply a real by the length of some list.
timesLengthOf :: (Integral b, Real a) => a -> [c] -> b
timesLengthOf u xs = floor $ realToFrac u * realToFrac (length xs)

