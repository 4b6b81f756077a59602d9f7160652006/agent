{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Agent.Data.Clock (
    Clock (..)
  , new
  , tick
  ) where

import           Data.Binary (Binary)
import           Data.Int (Int64)

import           GHC.Generics (Generic)


newtype Clock =
  Clock {
      getClock :: Int64
    } deriving (Bounded, Enum, Eq, Num, Ord, Show, Generic)

instance Binary Clock where

new :: Clock
new =
  Clock 0

tick :: Clock -> Clock
tick clock =
  clock + 1
