{-# LANGUAGE DeriveGeneric #-}
module Agent.Protocol (
    Initialise (..)
  , Complete (..)
  ) where

import           Control.Distributed.Process (ProcessId)

import           Data.Binary (Binary)

import           GHC.Generics (Generic)

data Initialise =
    Initialise
    deriving (Eq, Show, Generic)

instance Binary Initialise where

data Complete =
    Complete !ProcessId
    deriving (Eq, Ord, Show, Generic)

instance Binary Complete where
