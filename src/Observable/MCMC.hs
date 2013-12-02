{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Observable.MCMC where

import Control.Lens
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Vector (Vector)
import Observable.Core

data Target a = Target {
    _objective :: Vector a -> Double
  , _gradient  :: Maybe (Vector a -> Vector a)
  }

data Trace a = Trace {
    _parameterSpacePosition :: Vector a
  , _dataSpacePosition      :: Double
  , _optionalInformation    :: Double
  }

instance Show a => Show (Trace a) where
  show = show . _parameterSpacePosition

type Transition a = forall m. PrimMonad m =>
  Target a -> StateT (Trace a) (Observable m) (Vector a)

makeLenses ''Trace
makeLenses ''Target

-- | Target constructor using a gradient.
createTargetWithGradient
  :: (Vector a -> Double) -> (Vector a -> Vector a) -> Target a
createTargetWithGradient f g  = Target f (Just g)

-- | Target constructor sans gradient.
createTargetWithoutGradient :: (Vector a -> Double) -> Target a
createTargetWithoutGradient f = Target f Nothing

-- | Trace constructor.
initializeTrace :: Target a -> Vector a -> Double -> Trace a
initializeTrace t as = Trace as (t^.objective $ as)

-- | Sample from some distribution indirectly via MCMC.
observeIndirectly
  :: PrimMonad m
  => Target a
  -> Transition a
  -> Trace a
  -> Int
  -> Observable m (Trace a)
observeIndirectly f t o n = replicateM_ n (t f) `execStateT` o

-- | Better infix syntax for observeIndirectly.
observedIndirectlyBy
  :: PrimMonad m
  => Target a
  -> Transition a
  -> Trace a
  -> Int
  -> Observable m (Trace a)
observedIndirectlyBy = observeIndirectly

-- | Transition operator composition.
interleave :: Transition a -> Transition a -> Transition a
interleave t0 t1 target = t0 target >> t1 target

-- | Transition operator sampling.
randomlyInterleave :: Transition a -> Transition a -> Transition a
randomlyInterleave t0 t1 target = do
  s <- lift $ categorical [t0, t1]
  s target

-- | Simple gradient error handling.
handleGradient :: Maybe t -> t
handleGradient Nothing  = error "handleGradient: no gradient provided"
handleGradient (Just g) = g

