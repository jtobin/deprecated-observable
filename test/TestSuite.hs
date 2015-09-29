module Main where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.HashMap.Strict (empty)
import qualified Data.HashMap.Strict as HashMap
import Data.List
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import GHC.Word
import Observable.Core
import Observable.MCMC
import Observable.MCMC.Anneal
import Observable.MCMC.Hamiltonian
import Observable.MCMC.MALA
import Observable.MCMC.MetropolisHastings
import Observable.MCMC.NUTS
import Observable.MCMC.Slice
import Observable.Types
import System.IO
import System.Random.MWC hiding (Gen)
import Test.Hspec
import Test.QuickCheck hiding (vector, vectorOf, sample, frequency)

prettyPrint :: Handle -> Vector Double -> IO ()
prettyPrint h = hPutStrLn h . filter (`notElem` "fromList []") . show

-- targets --------------------------------------------------------------------

rosenbrock :: Target Double
rosenbrock = createTargetWithGradient lRosenbrock glRosenbrock where
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

himmelblau :: Target Double
himmelblau = createTargetWithGradient lHimmelblau glHimmelblau where
  lHimmelblau :: Vector Double -> Double
  lHimmelblau xs =
    let [x0, x1] = V.toList xs
    in  (-1) * ((x0 * x0 + x1 - 11) ^ 2 + (x0 + x1 * x1 - 7) ^ 2)

  glHimmelblau :: Vector Double -> Vector Double
  glHimmelblau xs =
    let [x, y] = V.toList xs
        quadFactor0 = x * x + y - 11
        quadFactor1 = x + y * y - 7
        dx = (-2) * (2 * quadFactor0 * x + quadFactor1)
        dy = (-2) * (quadFactor0 + 2 * quadFactor1 * y)
    in  V.fromList [dx, dy]

bnn :: Target Double
bnn = createTargetWithGradient lBnn glBnn where
  lBnn :: Vector Double -> Double
  lBnn xs =
    let [x0, x1] = V.toList xs
    in  -0.5 * (x0 ^ 2 * x1 ^ 2 + x0 ^ 2 + x1 ^ 2 - 8 * x0 - 8 * x1)

  glBnn :: Vector Double -> Vector Double
  glBnn xs =
    let [x, y] = V.toList xs
        dx = -0.5 * (2 * x * y * y + 2 * x - 8)
        dy = -0.5 * (2 * x * x * y + 2 * y - 8)
    in  V.fromList [x, y]

beale = createTargetWithGradient lBeale glBeale where
  lBeale :: Vector Double -> Double
  lBeale xs
      | and [x0 >= -4.5, x0 <= 4.5, x1 >= -4.5, x1 <= 4.5]
          = negate $ (1.5   - x0 + x0 * x1) ^ 2
                   + (2.25  - x0 + x0 * x1 ^ 2) ^ 2
                   + (2.625 - x0 + x0 * x1 ^ 3) ^ 2
      | otherwise = - (1 / 0)
    where
      [x0, x1] = V.toList xs

  glBeale :: Vector Double -> Vector Double
  glBeale xs =
    let [x0, x1] = V.toList xs
        dx = negate $ 2 * (1.5 - x0 + x0 * x1) * ((-1) + x1)
            + 2.25  * 2 * (2.25 - x0 + x0 * x1 ^ 2) * ((-1) + x1 ^ 2)
            + 2.625 * 2 * (2.2625 - x0 + x0 * x1 ^ 3) * ((-1) + x1 ^ 3)
        dy = negate $ 2 * (1.5 - x0 + x0 * x1) * x0
                    + 2 * (2.25 - x0 + x0 * x1 ^ 2) * 2 * x0 * x1
                    + 2 * (2.625 - x0 + x0 * x1 ^ 3) * 3 * x0 * x1 ^ 2
    in  V.fromList [dx, dy]

-- strategies -----------------------------------------------------------------

mhBaseStrategy :: PrimMonad m => Transition m Double
mhBaseStrategy = metropolisHastings (Just 1)

mhRadialStrategy :: PrimMonad m => Transition m Double
mhRadialStrategy =
  let radials = [0.1, 0.5, 1.0, 2.0, 2.5]
  in  interleave $ map (metropolisHastings . Just) radials

hmcBaseStrategy :: PrimMonad m => Transition m Double
hmcBaseStrategy = hamiltonian (Just 0.05) (Just 10)

malaBaseStrategy :: PrimMonad m => Transition m Double
malaBaseStrategy = mala (Just 0.15)

sliceStrategy :: PrimMonad m => Transition m Double
sliceStrategy = do
  slice 0.5
  slice 1.0
  oneOf [slice 4.0, slice 10.0]

nutsBaseStrategy :: PrimMonad m => Transition m Double
nutsBaseStrategy = do
  nuts

customStrategy :: PrimMonad m => Transition m Double
customStrategy = do
  firstWithProb 0.8
    (metropolisHastings (Just 3.0))
    (hamiltonian (Just 0.05) (Just 20))
  slice 3.0
  nuts

altCustomStrategy :: PrimMonad m => Transition m Double
altCustomStrategy = do
  firstWithProb 0.5
    mhBaseStrategy
    sliceBaseStrategy
  slice 3.0
  nuts

sliceBaseStrategy :: PrimMonad m => Transition m Double
sliceBaseStrategy = slice 1.0

randomStrategy :: PrimMonad m => Transition m Double
randomStrategy = frequency
  [ (5, metropolisHastings (Just 1.5))
  , (4, slice 1.0)
  , (1, nuts)
  ]

occasionallyJump :: PrimMonad m => Transition m Double
occasionallyJump = frequency
  [ (4, slice 1.0)
  , (2, mala (Just 0.15))
  , (1, nuts)
  ]

annealingStrategy :: PrimMonad m => Transition m Double
annealingStrategy = do
  anneal 0.70 $ randomStrategy
  anneal 0.05 $ randomStrategy
  anneal 0.05 $ randomStrategy
  anneal 0.70 $ randomStrategy
  randomStrategy

-- chains ---------------------------------------------------------------------

rosenbrockChain :: Chain Double
rosenbrockChain = Chain position target value empty where
  position = V.fromList [1.0, 1.0]
  target   = rosenbrock
  value    = logObjective target position

himmelblauChain :: Chain Double
himmelblauChain = Chain position target value empty where
  position = V.fromList [1.0, 1.0]
  target   = himmelblau
  value    = logObjective target position

bnnChain :: Chain Double
bnnChain = Chain position target value empty where
  position = V.fromList [1.0, 1.0]
  target   = bnn
  value    = logObjective target position

bealeChain :: Chain Double
bealeChain = Chain position target value empty where
  position = V.fromList [1.0, 1.0]
  target   = beale
  value    = logObjective target position

-- traces ---------------------------------------------------------------------

genericTrace :: Transition IO Double -> Chain Double -> Handle -> IO ()
genericTrace strategy chain h = create >>= \g ->
  traceChain 10000 strategy chain g >>= mapM_ (prettyPrint h)

annealingTrace :: Chain Double -> Handle -> IO ()
annealingTrace chain h = create >>= \g -> do
  let c = annealTransitions $ replicate 5000 (metropolisHastings (Just 1.0) )
  traceMarkovChain c chain g >>= mapM_ (prettyPrint h)

hmcTrace :: Chain Double -> Handle -> IO ()
hmcTrace = genericTrace hmcBaseStrategy

malaTrace :: Chain Double -> Handle -> IO ()
malaTrace = genericTrace malaBaseStrategy

sliceTrace :: Chain Double -> Handle -> IO ()
sliceTrace = genericTrace sliceStrategy

mhTrace :: Chain Double -> Handle -> IO ()
mhTrace = genericTrace mhRadialStrategy

nutsTrace :: Chain Double -> Handle -> IO ()
nutsTrace = genericTrace nutsBaseStrategy

customTrace :: Chain Double -> Handle -> IO ()
customTrace = genericTrace customStrategy

jumpTrace :: Chain Double -> Handle -> IO ()
jumpTrace = genericTrace occasionallyJump

annealTrace :: Chain Double -> Handle -> IO ()
annealTrace = genericTrace annealingStrategy

-- main -----------------------------------------------------------------------

mcmc = traceChain

trace :: Int -> Chain Double -> Transition IO Double -> Handle -> IO ()
trace n chain strategy h = do
  prng <- create
  samples <- mcmc n strategy chain prng
  mapM_ (prettyPrint h) samples

mhNutsStrategy :: PrimMonad m => Transition m Double
mhNutsStrategy = do
  firstWithProb 0.9
    (metropolisHastings (Just 0.1))
    nuts

mhMalaStrategy :: PrimMonad m => Transition m Double
mhMalaStrategy = do
  firstWithProb 0.9
    (metropolisHastings (Just 1.0))
    (mala (Just 0.1))

dissChain1 = trace 10000 rosenbrockChain mhBaseStrategy

dissChain2 = trace 2000 rosenbrockChain mhRadialStrategy

-- dissStrat3 = hmcBaseStrategy
-- dissChain3 = trace 10000 rosenbrockChain hmcBaseStrategy
--
-- dissStrat4 = sliceStrategy
-- dissChain4 = trace 10000 rosenbrockChain dissStrat4

tracer :: PrimMonad m => [(String, Int, Chain Double, Transition m Double)]
tracer =
    [ (sl ++ "-" ++ cl, n, chain, strat)
    | (cl, chain) <- chains
    , (sl, n, strat) <- strategies
    ]
  where
    strategies = [
        ("mh" , 10000, mhBaseStrategy)
      , ("mh-radial", 2000, mhRadialStrategy)
      , ("hmc", 10000, hmcBaseStrategy)
      , ("nuts", 10000, nutsBaseStrategy)
      , ("custom", 3333, customStrategy)
      , ("random", 10000, randomStrategy)
      , ("annealed", 2000, annealingStrategy)
      ]
    chains = [
        ("rosenbrock", rosenbrockChain)
      , ("beale", bealeChain)
      , ("himmelblau", himmelblauChain)
      , ("bnn", bnnChain)
      ]

handler (label, n, chain, strat) = do
  h <- openFile ("./test/" ++ label ++ ".dat") WriteMode
  trace n chain strat h
  hClose h

main :: IO ()
main = mapM_ handler tracer

  -- h <- openFile "./test/trace2.dat" WriteMode
  -- dissChain2 h
  -- hClose h

  -- h <- openFile "./test/trace3.dat" WriteMode
  -- dissChain3 h
  -- hClose h

  -- h <- openFile "./test/trace4.dat" WriteMode
  -- dissChain4 h
  -- hClose h

  -- h <- openFile "./test/trace5.dat" WriteMode
  -- dissChain5 h
  -- hClose h

  -- h <- openFile "./test/trace4.dat" WriteMode
  -- dissChain4 h
  -- hClose h

-- deprecated -----------------------------------------------------------------

vector :: (Eq a, Unbox a, Arbitrary a) => Gen (Vector a)
vector = V.fromList <$> listOf arbitrary

target ::  Gen (Target Double)
target = elements [rosenbrock, himmelblau, bnn]

spec_scalarTimesVector :: Spec
spec_scalarTimesVector = describe "(.*)" $ do
 it "returns the zero vector when multiplied by zero" $
   property $ do
     let zeros xs = V.fromList (replicate (V.length xs) 0)
     ds   <- vector :: Gen (Vector Double)
     return $ 0 .* ds == zeros ds

 it "scales vectors correctly" $
   property $ do
     vs <- vector `suchThat` (not . V.null) :: Gen (Vector Double)
     j  <- choose (0, V.length vs - 1)      :: Gen Int
     e  <- arbitrary                        :: Gen Double
     return $
       (e .* vs) V.! j == e * (vs V.! j)

spec_metropolisHastings :: Spec
spec_metropolisHastings = describe "metropolisHastings" $
  it "should move around the state space" $
    property $ do
      targetDist <- target
      mwcSeed    <- arbitrary :: Gen Word32
      e          <- choose (0.00001, 10)
      position   <- V.fromList <$> replicateM 2 (arbitrary :: Gen Double)
      let store = HashMap.insert MH (ODouble 0) HashMap.empty
          value = logObjective targetDist position
          chain = Chain position targetDist value store

      let tracked = runST $ do
            g     <- initialize (V.singleton mwcSeed)
            traceChain 100 (metropolisHastings (Just e)) chain g

      return $ length (nub tracked) > 1

runSpecs :: IO ()
runSpecs = hspec $ do
  spec_scalarTimesVector
  spec_metropolisHastings

