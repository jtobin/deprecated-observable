import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Observable.MCMC
import Observable.MCMC.MetropolisHastings
import Observable.Types

-- | The (log) Rosenbrock density.
lRosenbrock :: Vector Double -> Double
lRosenbrock xs =
  let [x0, x1] = V.toList xs
  in  (-1) * (5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

-- | The (log) Rosenbrock gradient.
glRosenbrock :: Vector Double -> Vector Double
glRosenbrock xs =
  let [x, y] = V.toList xs
      dx = 20 * x * (y - x ^ 2) + 0.1 * (1 - x)
      dy = -10 * (y - x ^ 2)
  in  V.fromList [dx, dy]

strategy :: PrimMonad m => Transition m Double
strategy = interleaveMany $ replicate 100 (metropolisHastings Nothing)

-- traceChain :: PrimMonad m => 0
-- traceChain = execStateT strategy

