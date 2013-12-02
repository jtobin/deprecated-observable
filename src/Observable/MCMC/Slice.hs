{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.Slice where

import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V hiding (length)
import Observable.Core
import Observable.MCMC
import System.Random.MWC hiding (uniform)

-- | A multivariate slice sampler.
slice :: Double -> TransitionOperator Double
slice e t = do
  currentPosition <- use parameterSpacePosition
  let n = V.length currentPosition
  forM_ [0..n - 1] $ \j -> do
    q       <- use parameterSpacePosition
    height  <- liftM log   $ lift $ uniform (0, exp $ t^.objective $ q)
    bracket <- lift . lift $ findBracket (t^.objective) j e height q
    next    <- lift        $ rejection   (t^.objective) j bracket height q
    put $ Trace next (t^.objective $ next) e

  use parameterSpacePosition
    
-- | Find a bracket around a density at a point.
findBracket :: (Num b, Ord a, PrimMonad m) 
  => (Vector b -> a)
  -> Int
  -> b
  -> a
  -> Vector b
  -> m (b, b)
findBracket f j e height xs = go xs xs where
  go !bl !br
    | f bl <  height && f br <  height =
        return (bl `V.unsafeIndex` j , br `V.unsafeIndex` j)
    | f bl <  height && f br >= height = do
        br0 <- expandRight j e br
        go bl br0 
    | f bl >= height && f br <  height = do
        bl0 <- expandLeft j e bl
        go bl0 br
    | otherwise = do
        bl0 <- expandLeft  j e bl
        br0 <- expandRight j e br
        go bl0 br0

-- | Expand a bracket by some means.
expandBy
  :: PrimMonad m => (a -> a -> a) -> Int -> a -> Vector a -> m (Vector a)
expandBy f j e xs = do
  v  <- V.thaw xs
  xj <- V.unsafeRead v j
  V.unsafeWrite v j (f xj e)
  V.freeze v

-- | Expand a bracket to the right.
expandRight :: (Num a, PrimMonad m) => Int -> a -> Vector a -> m (Vector a)
expandRight = expandBy (+)

-- | Expand a bracket to the left.
expandLeft :: (Num a, PrimMonad m) => Int -> a -> Vector a -> m (Vector a)
expandLeft  = expandBy (-)

-- | Rejection sample along a bracket.
rejection :: (Ord b, PrimMonad m, Variate a) =>
  (Vector a -> b) -> Int -> (a, a) -> b -> Vector a -> Observable m (Vector a)
rejection f j bracket height xs = go xs where
  go zs = do
    u    <- uniform bracket
    v    <- lift $ V.thaw zs
    lift $ V.unsafeWrite v j u
    cool <- lift $ V.freeze v
    if   f cool < height
    then go cool
    else return cool

