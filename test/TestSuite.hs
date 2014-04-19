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
import Observable.MCMC.Anneal
import Observable.MCMC.Hamiltonian
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

nutsStrategy :: PrimMonad m => Transition m Double
nutsStrategy = do
  nuts

customStrategy :: PrimMonad m => Transition m Double
customStrategy = do
  firstWithProb 0.8
    (metropolisHastings (Just 3.0))
    (hamiltonian (Just 0.05) (Just 20))
  slice 3.0
  nuts

-- cool!
annealingStrategy :: PrimMonad m => Transition m Double
annealingStrategy = do
  let transition = frequency
        [ (5, metropolisHastings (Just 1.5))
        , (4, slice 1.0)
        , (1, nuts)
        ]
  anneal 0.70 $ transition
  anneal 0.05 $ transition
  anneal 0.01 $ transition
  anneal 0.05 $ transition
  anneal 0.70 $ transition
  transition

occasionallyJump :: PrimMonad m => Transition m Double
occasionallyJump = frequency
  [ (4, slice 1.0)
  , (1, nuts)
  ]

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

genericTrace :: Transition IO Double -> Chain Double -> Handle -> IO ()
genericTrace strategy chain h = withSystemRandom . asGenIO $ \g ->
  traceChain 5000 strategy chain g >>= mapM_ (prettyPrint h)

hmcTrace :: Chain Double -> Handle -> IO ()
hmcTrace = genericTrace hmcStrategy

sliceTrace :: Chain Double -> Handle -> IO ()
sliceTrace = genericTrace sliceStrategy

mhTrace :: Chain Double -> Handle -> IO ()
mhTrace = genericTrace mhStrategy

nutsTrace :: Chain Double -> Handle -> IO ()
nutsTrace = genericTrace nutsStrategy

customTrace :: Chain Double -> Handle -> IO ()
customTrace = genericTrace customStrategy

jumpTrace :: Chain Double -> Handle -> IO ()
jumpTrace = genericTrace occasionallyJump

annealTrace :: Chain Double -> Handle -> IO ()
annealTrace = genericTrace annealingStrategy

main :: IO ()
main = withSystemRandom . asGenIO $ \g -> do
  let chains = [rosenbrockChain, himmelblauChain, bnnChain, bealeChain]
      chain  = himmelblauChain
  -- chain <- observe (categorical chains) g

  h <- openFile "./test/trace.dat" WriteMode
  annealTrace chain h
  hClose h

