{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.Anneal (anneal) where

import Control.Monad.Primitive
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Observable.Types

-- experimental
-- this needs to be handled differently, maybe by way of the store..

anneal :: PrimMonad m => Double -> Transition m Double -> Transition m Double
anneal temperature baseTransition = do
  Chain next target@(Target l mg) _ store <- get
  let l' xs = temperature * l xs
      g' = case mg of
             Nothing -> Nothing
             Just g  -> Just (V.map (* temperature) . g)
      target' = Target l' g'

  modify (\(Chain n _ _ s) -> Chain n target' (l' next) s)

  baseTransition

  modify (\(Chain n _ _ s) -> Chain n target (logObjective target n) s)
  Chain n _ _ _ <- get
  return n

