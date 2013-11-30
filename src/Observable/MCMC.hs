{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Observable.MCMC where

import Control.Lens
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Observable.Core

data Target a = Target {
    _objective :: [a] -> Double
  , _gradient  :: Maybe ([a] -> [a])
  }

data Trace a e = Trace {
    _parameterSpacePosition :: [a]
  , _dataSpacePosition      :: Double
  , _optionalInformation    :: e
  }

instance Show a => Show (Trace a e) where
  show = show . _parameterSpacePosition

type TransitionOperator a e =
  forall m. PrimMonad m => Target a -> StateT (Trace a e) (Observable m) [a]

makeLenses ''Trace
makeLenses ''Target

createTargetWithGradient :: ([a] -> Double) -> ([a] -> [a]) -> Target a
createTargetWithGradient f g  = Target f (Just g)

createTargetWithoutGradient :: ([a] -> Double) -> Target a
createTargetWithoutGradient f = Target f Nothing

initializeTrace :: Target a -> [a] -> e -> Trace a e
initializeTrace t as = Trace as (t^.objective $ as)

-- | Sample from some distribution indirectly via MCMC.
observeIndirectly :: Monad m => t -> (t -> StateT s m a) -> s -> Int -> m s
observeIndirectly f t o n = replicateM_ n (t f) `execStateT` o

-- | Better infix syntax for observeIndirectly.
observedIndirectlyBy :: Monad m => t -> (t -> StateT s m a) -> s -> Int -> m s
observedIndirectlyBy = observeIndirectly

-- | Transition operator composition.
interleave :: Monad m => (t -> m a) -> (t -> m b) -> t -> m b
interleave t0 t1 target = t0 target >> t1 target

handleGradient :: Maybe t -> t
handleGradient Nothing  = error "handleGradient: no gradient provided"
handleGradient (Just g) = g

