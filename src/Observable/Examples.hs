import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Lens
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Observable.MCMC
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
--   DAG-sampling syntax.
betaBinomial :: Int -> Double -> Double -> Observable IO Int
betaBinomial n a b = do
  p <- beta a b
  binomial n p

-- | Log-Rosenbrock function.
lRosenbrock :: Vector Double -> Double
lRosenbrock xs = let [x0, x1] = V.toList xs
                 in  (-1) * (5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

-- | Gradient of log-Rosenbrock.
glRosenbrock :: Vector Double -> Vector Double
glRosenbrock xs =
  let [x, y] = V.toList xs
      dx = 20 * x * (y - x ^ 2) + 0.1 * (1 - x)
      dy = -10 * (y - x ^ 2)
  in  V.fromList [dx, dy]

lRosenbrockTarget :: Target Double
lRosenbrockTarget = createTargetWithGradient lRosenbrock glRosenbrock

customTransition :: Transition Double
customTransition =     metropolisHastings 0.5
  `randomlyInterleave` nuts 0.1
  `randomlyInterleave` slice 0.4
  `interleave`         metropolisHastings 0.1

origin  = initializeChain lRosenbrockTarget [0.0, 0.0] 0.5

logRosenbrock :: PrimMonad m => Observable m (MarkovChain Double)
logRosenbrock = withMcmc customTransition origin 100 lRosenbrockTarget

main :: IO ()
main = do
  -- Can examine a single chain for diagnostic purposes or interest's sake
  -- zs <- withSystemRandom . asGenIO $
  --         traceChain customTransition origin 1000 lRosenbrockTarget

  -- Or sample (approximately) independently from the equilibrium distribution 
  -- in parallel, provided the requisite iterations are sufficient for burn-in.

  zs <- sampleConcurrently 1000 logRosenbrock

  mapM_ print zs

