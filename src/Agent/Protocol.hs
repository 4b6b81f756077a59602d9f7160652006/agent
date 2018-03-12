{-# LANGUAGE DeriveGeneric #-}
module Agent.Protocol (
    Initialise (..)
  , Complete (..)
  ) where

import           Agent.Data.Log (Log (..))
import           Agent.Data.Random (Seed (..))
import           Agent.Data.Timer (Duration (..))

import           Control.Distributed.Process (ProcessId)

import           Data.Binary (Binary)

import           GHC.Generics (Generic)

data Initialise =
    Initialise [ProcessId] Duration Duration Seed
    deriving (Eq, Show, Generic)

instance Binary Initialise where

data Replicate =
    Replicate (Log ProcessId)
    deriving (Eq, Show, Generic)

instance Binary Replicate where

data Complete =
    Complete !ProcessId Int Double
    deriving (Eq, Ord, Show, Generic)

instance Binary Complete where
