import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Lens
import Data.Vector (Vector)
import qualified Data.Vector as V
import Observable.Core
import Observable.MCMC
import Observable.MCMC.MetropolisHastings
import Observable.MCMC.NUTS
import Observable.MCMC.Slice
import Pipes
import System.Random.MWC

import Debug.Trace

-- | Result of a coin toss.
data Toss = Head | Tail deriving Show

-- | Toss a coin in the IO monad.
toss :: Double -> Observable IO Toss
toss p = do
  b <- bernoulli p
  return $ if b then Head else Tail

-- | Toss ten biased coins in the IO monad.
tossTenBiased :: Observable IO [Toss]
tossTenBiased = replicateM 10 (toss 0.2)

-- | Toss ten coins and print the resulting list to stdout.
randomDraw :: IO [Toss]
randomDraw = withSystemRandom . asGenIO $ sample tossTenBiased

-- | Beta-binomial in the IO monad.
betaBinomial :: Int -> Double -> Double -> Observable IO Int
betaBinomial n a b = do
  p <- beta a b
  binomial n p

-- | Here's something weird..
rosenbrockBinomial :: Int -> Observable IO Int
rosenbrockBinomial n = do
  rosenbrockDraw <- logRosenbrockVariate q0 100
  let rawP = exp <$> (rosenbrockDraw^.parameterSpacePosition)
      p    = V.last rawP / V.sum rawP
  binomial n p 

-- | log-Rosenbrock function.
lRosenbrock :: RealFloat a => Vector a -> a
lRosenbrock xs = let [x0, x1] = V.toList xs
                 in  (-1) * (5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

-- | Gradient of log-Rosenbrock.
glRosenbrock :: RealFloat a => Vector a -> Vector a
glRosenbrock xs =
  let [x, y] = V.toList xs
      dx = 20 * x * (y - x ^ 2) + 0.1 * (1 - x)
      dy = -10 * (y - x ^ 2)
  in  V.fromList [dx, dy]

logRosenbrock :: Target Double
logRosenbrock = createTargetWithGradient lRosenbrock glRosenbrock

compositeMetropolis :: TransitionOperator Double
compositeMetropolis = metropolisHastings 1.0 
         `interleave` metropolisHastings 0.5 
         `interleave` metropolisHastings 0.1 

compositeTransition :: TransitionOperator Double
compositeTransition = metropolisHastings 0.5
         `interleave` nuts 0.1
         `interleave` slice 0.4

-- you ideally want to get rid of the 'trace' at this point.. hmmm
logRosenbrockVariate
  :: PrimMonad m
  => Trace Double -> Int -> Observable m (Trace Double)
logRosenbrockVariate = logRosenbrock `observedIndirectlyBy` compositeTransition
 
q0 = Trace (V.fromList [0.0, 0.0]) (lRosenbrock (V.fromList [0.0, 0.0])) 0.5

main = withSystemRandom . asGenIO $ \g ->
  -- sampleIO (logRosenbrockVariate q0 100) g >>= print
  runEffect $ observe (logRosenbrockVariate q0 100) g >-> display
