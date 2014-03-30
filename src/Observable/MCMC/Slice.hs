{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.Slice where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as V hiding (length)
import Observable.Core
import Observable.Types
import System.Random.MWC hiding (uniform)

-- | A multivariate slice sampler.
slice :: PrimMonad m => Double -> Transition m Double
slice e = do
  Chain position target _ _ <- get
  let n = V.length position
  forM_ [0..n - 1] $ \j -> do
    Chain q _ _ _ <- get
    height  <- liftM log   $ lift $ uniform (0, exp $ logObjective target q)
    bracket <- lift . lift $ findBracket (logObjective target) j e height q
    next    <- lift $ rejection   (logObjective target) j bracket height q
    put $ Chain next target (logObjective target next) undefined

  Chain q _ _ _ <- get
  return q
    
-- | Find a bracket around a density at a point.
--
--   NOTE obviously want to improve the efficiency of this guy.  The linear
--        search is pretty hacky.
findBracket :: (Num b, Ord a, PrimMonad m, Unbox b)
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
  :: (PrimMonad m, Unbox a)
  => (a -> a -> a) -> Int -> a -> Vector a -> m (Vector a)
expandBy f j e xs = do
  v  <- V.thaw xs
  xj <- V.unsafeRead v j
  V.unsafeWrite v j (f xj e)
  V.freeze v

-- | Expand a bracket to the right.
expandRight
  :: (Num a, Unbox a, PrimMonad m)
  => Int -> a -> Vector a -> m (Vector a)
expandRight = expandBy (+)

-- | Expand a bracket to the left.
expandLeft
  :: (Unbox a, Num a, PrimMonad m)
  => Int -> a -> Vector a -> m (Vector a)
expandLeft  = expandBy (-)

-- | Rejection sample along a bracket.
rejection
  :: (Ord b, Unbox a, PrimMonad m, Variate a)
  => (Vector a -> b)
  -> Int
  -> (a, a)
  -> b
  -> Vector a
  -> Observable m (Vector a)
rejection f j bracket height = go where
  go zs = do
    u    <- uniform bracket
    v    <- lift $ V.thaw zs
    lift $ V.unsafeWrite v j u
    cool <- lift $ V.freeze v
    if   f cool < height
    then go cool
    else return cool

