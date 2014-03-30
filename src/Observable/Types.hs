{-# LANGUAGE DeriveGeneric #-}

module Observable.Types where

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import GHC.Generics (Generic)
import System.Random.MWC

newtype Observable m a = Observable { sample :: Gen (PrimState m) -> m a }

instance Monad m => Functor (Observable m) where
  fmap h (Observable f) = Observable $ liftM h . f

instance Monad m => Applicative (Observable m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (Observable m) where
  return x = Observable $ const (return x)
  m >>= h  = Observable $ \g -> do
    z <- sample m g
    sample (h z) g

instance MonadTrans Observable where
  lift m = Observable $ const m

data Target a = Target {
    logObjective :: Vector a -> Double
  , gradient     :: Maybe (Vector a -> Vector a)
  }

createTargetWithGradient
  :: (Vector a -> Double) -> (Vector a -> Vector a) -> Target a
createTargetWithGradient f g  = Target f (Just g)

createTargetWithoutGradient :: (Vector a -> Double) -> Target a
createTargetWithoutGradient f = Target f Nothing

handleGradient :: Maybe t -> t
handleGradient Nothing  = error "handleGradient: no gradient provided"
handleGradient (Just g) = g

data Algorithm = MH | HMC deriving (Eq, Show, Generic)

instance Hashable Algorithm

type OptionalStore = HashMap Algorithm OptionalInfo

data OptionalInfo = 
    ODouble Double
  | OInt Int
  | OPair (OptionalInfo, OptionalInfo)
  deriving (Eq, Show)

data Chain a = Chain {
    parameterSpacePosition :: Vector a
  , objectiveFunction      :: Target a
  , objectiveValue         :: Double
  , optionalInformation    :: OptionalStore
  }

instance (Show a, Unbox a) => Show (Chain a) where
  show = show . parameterSpacePosition

type Transition m a = StateT (Chain a) (Observable m) (Vector a)

