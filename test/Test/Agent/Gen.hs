module Test.Agent.Gen (
    duration
  , seed
  ) where

import           Agent.Data.Timer (Duration (..))
import           Agent.Data.Random (Seed (..))

import           Hedgehog hiding (Seed)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

duration :: Gen Duration
duration =
  Duration <$> Gen.int (Range.linear 0 1000)

seed :: Gen Seed
seed =
  Seed <$> Gen.word32 (Range.linear 0 99)
