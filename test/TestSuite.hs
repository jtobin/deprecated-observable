module TestSuite where

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
import Observable.MCMC.MetropolisHastings
import Observable.MCMC.Hamiltonian
import Observable.MCMC.Slice
import Observable.Types
import System.Random.MWC hiding (Gen)
import Test.Hspec
import Test.QuickCheck hiding (vector, vectorOf, sample)

prettyPrint :: Vector Double -> IO ()
prettyPrint = putStrLn . filter (`notElem` "fromList []") . show

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

himmelblau :: Target Double
himmelblau = createTargetWithGradient lHimmelblau glHimmelblau

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

bnn :: Target Double
bnn = createTargetWithGradient lBnn glBnn

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

-- spec_hamiltonian :: Spec
-- spec_hamiltonian = describe "hamiltonian" $ 
--   it "should move around the state space" $ 
--     property $ do
--       targetDist <- target
--       mwcSeed    <- arbitrary :: Gen Word32
--       l          <- choose (10 :: Int, 50)
--       e          <- choose (0.01, 0.5)
--       position   <- V.fromList <$> replicateM 2 (choose (0.0, 1.0))
--       let store = HashMap.insert HMC (ODouble e) HashMap.empty
--           value = logObjective targetDist position
--           chain = Chain position targetDist value store
-- 
--       let tracked = runST $ do
--             g     <- initialize (V.singleton mwcSeed)
--             traceChain 100 (hamiltonian (Just e) l) chain g
-- 
--       return $ length (nub tracked) > 1


runSpecs :: IO ()
runSpecs = hspec $ do
  spec_scalarTimesVector
  spec_metropolisHastings
  -- spec_hamiltonian

-- other stuff to be reintegrated

mhStrategy :: PrimMonad m => Transition m Double
mhStrategy = 
  let radials = [0.1, 0.5, 1.0, 2.0, 2.5]  
  in  interleave $ map (metropolisHastings . Just) radials 

hmcStrategy :: PrimMonad m => Transition m Double
hmcStrategy = do
  hamiltonian (Just 0.01) (Just 10)
  hamiltonian (Just 0.05) (Just 10)
  hamiltonian (Just 0.10) (Just 10)

sliceStrategy :: PrimMonad m => Transition m Double
sliceStrategy = do
  slice 0.5
  slice 1.0
  oneOf [slice 4.0, slice 10.0]

customStrategy :: PrimMonad m => Transition m Double
customStrategy = do
  firstWithProb 0.8
    (metropolisHastings (Just 3.0))
    (hamiltonian (Just 0.05) (Just 20))
  slice 3.0

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

hmcTrace :: Chain Double -> IO ()
hmcTrace chain = withSystemRandom . asGenIO $ \g -> do
  cs <- traceChain 10000 hmcStrategy chain g
  mapM_ prettyPrint cs

sliceTrace :: Chain Double -> IO ()
sliceTrace chain = withSystemRandom . asGenIO $ \g -> do
  cs <- traceChain 10000 sliceStrategy chain g
  mapM_ prettyPrint cs

customTrace :: Chain Double -> IO ()
customTrace chain = withSystemRandom . asGenIO $ \g -> do
  cs <- traceChain 10000 customStrategy chain g
  mapM_ prettyPrint cs

mhTrace :: Chain Double -> IO ()
mhTrace chain = withSystemRandom . asGenIO $ \g -> do
  cs <- traceChain 10000 mhStrategy chain g
  mapM_ prettyPrint cs

spec_leapfrogIntegrator = 
  let t0 = V.fromList [1.0, 1.0]
      r0 = V.fromList [-1.1, -0.34]
      e  = 0.01
  in  mapM_ (prettyPrint . fst . leapfrogIntegrator rosenbrock t0 r0 e) [1..50]

-- just run a random trace
main = withSystemRandom . asGenIO $ \g -> do
  chain <- sample (categorical [rosenbrockChain, himmelblauChain, bnnChain]) g
  customTrace chain

