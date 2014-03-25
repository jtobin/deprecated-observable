import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Control.Lens
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Observable.MCMC
import System.Random.MWC

-- Coin tossing ---------------------------------------------------------------

data Toss = Head | Tail deriving Show

boolToToss :: Bool -> Toss
boolToToss True  = Head
boolToToss False = Tail

tossWithBias :: Double -> Observable IO Toss
tossWithBias p = boolToToss <$> bernoulli p

coinTossExample :: IO [Toss]
coinTossExample = sampleIO $ replicateM 10 (tossWithBias 0.2)

-- Rosenbrock density ---------------------------------------------------------

lRosenbrock :: Vector Double -> Double
lRosenbrock xs =
  let [x0, x1] = V.toList xs
  in  (-1) * (5 * (x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

glRosenbrock :: Vector Double -> Vector Double
glRosenbrock xs =
  let [x, y] = V.toList xs
      dx = 20 * x * (y - x ^ 2) + 0.1 * (1 - x)
      dy = -10 * (y - x ^ 2)
  in  V.fromList [dx, dy]

rosenbrock :: Target Double
rosenbrock = createTargetWithGradient lRosenbrock glRosenbrock

origin :: MarkovChain Double
origin = initializeChain rosenbrock [0.0, 0.0] 0.0

printWithoutBrackets :: Show a => a -> IO ()
printWithoutBrackets = putStrLn . filter (`notElem` "fromList[] ") . show

rosenbrockExample :: IO ()
rosenbrockExample = do
  trace <- withSystemRandom . asGenIO $
    traceChain rosenbrock (slice 0.35) origin 20000

  mapM_ printWithoutBrackets trace

-- Annealing ------------------------------------------------------------------

logDoublePotentialWell :: Double -> Vector Double -> Double
logDoublePotentialWell g xs = let [x] = V.toList xs
                              in  (-1) * g * (x ^ 2 - 1) ^ 2

doublePotentialWell = createTargetWithoutGradient $ logDoublePotentialWell 4

nOrigin = initializeChain doublePotentialWell [-1.0] 0.0

annealExample :: IO ()
annealExample = do
  trace <- withSystemRandom . asGenIO $
    traceChain doublePotentialWell (metropolisHastings 0.1) nOrigin 10000

  mapM_ printWithoutBrackets trace


-- annealer
--
-- chains=function(pot=U, tune=0.1, init=1)
-- {
--   x=rep(init,length(temps))
--   xmat=matrix(0,iters,length(temps))
--   for (i in 1:iters) {
--     can=x+rnorm(length(temps),0,tune)
--     logA=unlist(Map(pot,temps,x))-unlist(Map(pot,temps,can))
--     accept=(log(runif(length(temps)))<logA)
--     x[accept]=can[accept]
--     # now the coupling update
--     swap=sample(1:length(temps),2)
--     logA=pot(temps[swap[1]],x[swap[1]])+pot(temps[swap[2]],x[swap[2]])-
--             pot(temps[swap[1]],x[swap[2]])-pot(temps[swap[2]],x[swap[1]])
--     if (log(runif(1))<logA)
--       x[swap]=rev(x[swap])
--     # end of the coupling update
--     xmat[i,]=x
--   }
--   colnames(xmat)=paste("gamma=",temps,sep="")
--   xmat
-- }
  
exampleStrategy :: Transition Double
exampleStrategy = interleaveMany [nuts 1.0, metropolisHastings 2.0]



main :: IO ()
main = annealExample


