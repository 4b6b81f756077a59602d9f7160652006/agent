module Test.Agent.Gen (
    duration
  ) where

import           Agent.Data.Timer (Duration (..))

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

duration :: Gen Duration
duration =
  Duration <$> Gen.int (Range.linear 0 1000)
