module Test.Agent.Gen (
  -- * Clock
    clock

  -- * Log
  , value
  , event
  , events

  -- * MatrixClock
  , matrixClock

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
import           Agent.Data.Log (Value (..), Event (..))
import           Agent.Data.Random (Seed (..))
import           Agent.Data.Timer (Duration (..))
import           Agent.Data.VectorClock (VectorClock (..))
import qualified Agent.Data.VectorClock as VectorClock
import           Agent.Data.MatrixClock (MatrixClock (..))
import qualified Agent.Data.MatrixClock as MatrixClock

import qualified Data.List as List

import           Hedgehog hiding (Seed)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


clock :: Gen Clock
clock =
  Clock <$> Gen.int64 (Range.linear 0 1000)

value :: Gen Value
value =
  Value <$> Gen.double (Range.linearFrac 0 1)

event :: [Char] -> Gen (Event Char)
event ps =
  Event <$> clock <*> Gen.element ps <*> value

events :: [Char] -> Gen [Event Char]
events ps = do
  Gen.list (Range.linear 0 100) (event ps)

matrixClock :: Gen (MatrixClock Char)
matrixClock = do
  n <- Gen.int (Range.linear 0 26)
  let ps = List.take n ['a'..'z']
  v <- List.zip ['a'..'z'] <$> Gen.list (Range.linear 0 26) (vectorClockFor ps)
  pure $ MatrixClock.fromList v

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
