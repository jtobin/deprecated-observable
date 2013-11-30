import Control.Monad
import Control.Monad.Primitive
import Observable.Core
import Observable.MCMC
import Observable.MCMC.Hamiltonian
import Observable.MCMC.MetropolisHastings
import Observable.MCMC.NUTS
import Pipes
import System.Random.MWC

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

compositeMetropolis :: TransitionOperator Double Double
compositeMetropolis = metropolisTransition 1.0 
         `interleave` metropolisTransition 0.5 
         `interleave` metropolisTransition 0.1 

hmc :: TransitionOperator Double Double
hmc = hamiltonianTransition 1

nuts :: TransitionOperator Double Double
nuts = nutsTransition 0.1

compositeTransition :: TransitionOperator Double Double
compositeTransition = metropolisTransition 0.5
         `interleave` nutsTransition 0.1
         `interleave` metropolisTransition 0.1
         `interleave` nutsTransition 0.5

-- you ideally want to get rid of the 'trace' at this point.. hmmm
logRosenbrockVariate
  :: PrimMonad m
  => Trace Double Double -> Int -> Observable m (Trace Double Double)
logRosenbrockVariate = logRosenbrock `observedIndirectlyBy` compositeTransition
 
q0 = Trace [0.0, 0.0] (lRosenbrock [0.0, 0.0]) 0.5

main = do
  withSystemRandom . asGenIO $ \g ->
    runEffect $ observe (logRosenbrockVariate q0 100) g >-> printer

