module Test.Agent.Gen (
  -- Clock
    clock
  -- Random
  , seed
  -- Timer
  , duration
  ) where


import           Agent.Data.Clock (Clock (..))
import           Agent.Data.Random (Seed (..))
import           Agent.Data.Timer (Duration (..))

import           Hedgehog hiding (Seed)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

clock :: Gen Clock
clock =
  Clock <$> Gen.int64 (Range.linear 0 1000)

seed :: Gen Seed
seed =
  Seed <$> Gen.word32 (Range.linear 0 99)

duration :: Gen Duration
duration =
  Duration <$> Gen.int (Range.linear 0 1000)
