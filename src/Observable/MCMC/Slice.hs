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

-- needs to be tested
slice :: Double -> TransitionOperator Double
slice e t = do
  currentPosition <- use parameterSpacePosition
  let n = V.length currentPosition
  forM_ [0..n - 1] $ \j -> do
    q       <- use parameterSpacePosition
    bracket <- lift $ lift $ findBracket (t^.objective) j e q
    next    <- lift $ rejection (t^.objective) j bracket q
    put $ Trace next (t^.objective $ next) e

  use parameterSpacePosition
    

findBracket :: (Num b, Ord a, PrimMonad m) 
  => (Vector b -> a)
  -> Int
  -> b
  -> Vector b
  -> m (b, b)
findBracket f j e xs = go xs xs where
  f0 = f xs
  go !bl !br
    | f bl >  f0 && f br >  f0 =
        return (bl `V.unsafeIndex` j , br `V.unsafeIndex` j)
    | f bl >  f0 && f br <= f0 = do
        br0 <- expandRight j e br
        go bl br0 
    | f bl <= f0 && f br >  f0 = do
        bl0 <- expandLeft j e bl
        go bl0 br
    | otherwise = do
        bl0 <- expandLeft  j e bl
        br0 <- expandRight j e br
        go bl0 br0

expandBy
  :: PrimMonad m => (a -> t -> a) -> Int -> t -> Vector a -> m (Vector a)
expandBy f j e xs = do
  let xj = V.unsafeIndex xs j
  xs0 <- V.unsafeThaw xs
  V.unsafeWrite xs0 j (f xj e)
  V.unsafeFreeze xs0

expandRight :: (Num a, PrimMonad m) => Int -> a -> Vector a -> m (Vector a)
expandRight = expandBy (+)

expandLeft :: (Num a, PrimMonad m) => Int -> a -> Vector a -> m (Vector a)
expandLeft  = expandBy (-)

rejection :: (Ord b, PrimMonad m, Variate a) =>
  (Vector a -> b) -> Int -> (a, a) -> Vector a -> Observable m (Vector a)
rejection f j bracket xs = let xsCopy = xs in go xsCopy where
  f0 = f xs
  go zs = do
    u <- uniform bracket
    v <- lift $ V.unsafeThaw zs
    lift $ V.unsafeWrite v j u
    cool <- lift $ V.unsafeFreeze v
    if   f cool > f0
    then go cool
    else return cool






-- rejection f base bracket = go where
--   f0 = f base
--   go = do
--     u <- uniform bracket
--     if   f u > f0
--     then go
--     else return u
  


