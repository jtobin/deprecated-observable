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

-- | Beta-binomial in the IO monad.  Note that do-notation forms a natural
--   Gibbs sampling syntax.  It should also be possible to automatically turn
--   this into a chromatic sampler via some low-level hacking.
betaBinomial :: Int -> Double -> Double -> Observable IO Int
betaBinomial n a b = do
  p <- beta a b
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

compositeTransition :: Transition Double
compositeTransition = metropolisHastings 0.5
         `interleave` nuts 0.1
         `interleave` slice 0.4

noisyTransition :: Transition Double
noisyTransition =      metropolisHastings 0.5
  `randomlyInterleave` nuts 0.1
  `randomlyInterleave` slice 0.4
  `interleave`         metropolisHastings 0.1

logRosenbrockVariate :: PrimMonad m => Observable m (Trace Double)
logRosenbrockVariate =
    observeIndirectly logRosenbrock noisyTransition q0 100
  where q0 = initializeTrace logRosenbrock (V.fromList [0.0, 0.0]) 0.5

main :: IO ()
main = do
  zs <- sampleConcurrently 1000 logRosenbrockVariate
  mapM_ print zs

