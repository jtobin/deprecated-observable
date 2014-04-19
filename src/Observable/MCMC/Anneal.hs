{-# LANGUAGE RankNTypes #-}

module Observable.MCMC.Anneal (anneal) where

import Control.Monad.Primitive
import Control.Monad.State.Strict
import qualified Data.Vector.Unboxed as V
import Observable.Core
import Observable.Types

anneal :: PrimMonad m => Double -> Transition m Double -> Transition m Double
anneal invTemp baseTransition
  | invTemp < 0 = error "anneal: invalid temperture"
  | otherwise = do
      Chain next target@(Target l mg) _ store <- get
      let l' xs = invTemp * l xs
          g' = case mg of
                 Nothing -> Nothing
                 Just g  -> Just (V.map (* invTemp) . g)
          target' = Target l' g'

      modify (\(Chain n _ _ s) -> Chain n target' (l' next) s)

      baseTransition

      modify (\(Chain n _ _ s) -> Chain n target (logObjective target n) s)
      Chain n _ _ _ <- get
      return n

