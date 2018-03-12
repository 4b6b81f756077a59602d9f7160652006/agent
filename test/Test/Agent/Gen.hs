module Test.Agent.Gen (
  -- * Clock
    clock

  -- * Random
  , seed

  -- * Test Processes
  , process
  , processes

  -- * Timer
  , duration

  -- * VectorClock
  , vectorClock
  , vectorClockFor
  ) where


import           Agent.Data.Clock (Clock (..))
import           Agent.Data.Random (Seed (..))
import           Agent.Data.Timer (Duration (..))
import           Agent.Data.VectorClock (VectorClock (..))
import qualified Agent.Data.VectorClock as VectorClock

import qualified Data.List as List

import           Hedgehog hiding (Seed)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

clock :: Gen Clock
clock =
  Clock <$> Gen.int64 (Range.linear 0 1000)

seed :: Gen Seed
seed =
  Seed <$> Gen.word32 (Range.linear 0 99)

process :: Gen Char
process =
  Gen.element ['a'..'z']

processes :: Gen [Char]
processes = do
  n <- Gen.int (Range.linear 0 26)
  pure $ List.take n ['a'..'z']

duration :: Gen Duration
duration =
  Duration <$> Gen.int (Range.linear 0 1000)

vectorClock :: Gen (VectorClock Char)
vectorClock = do
  n <- Gen.int (Range.linear 0 26)
  vectorClockFor (List.take n ['a'..'z'])

vectorClockFor :: [Char] -> Gen (VectorClock Char)
vectorClockFor ps = do
  clocks <- List.zip ps <$> Gen.list (Range.singleton (List.length ps)) clock
  pure $ List.foldl (\acc (p, c) -> VectorClock.set p c acc) VectorClock.new clocks
