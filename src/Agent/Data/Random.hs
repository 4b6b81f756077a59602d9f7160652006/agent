{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Agent.Data.Random (
    Seed (..)
  , Random (..)
  , new
  , next
  ) where

import           Control.Monad.Primitive (PrimState)
import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Binary (Binary)
import           Data.Word (Word32)
import qualified Data.Vector as Vector

import           GHC.Generics (Generic)

import qualified System.Random.MWC as Random

newtype Seed =
  Seed {
      getSeed :: Word32
    } deriving (Eq, Ord, Show, Generic)

instance Binary Seed where

newtype Random =
  Random {
      getGen :: Random.Gen (PrimState IO)
    }

new :: MonadIO m => Seed -> m Random
new (Seed seed) =
  liftIO $
    Random <$> Random.initialize (Vector.singleton seed)

next :: MonadIO m => Random -> m Double
next (Random gen) =
  liftIO $
    Random.uniform gen
