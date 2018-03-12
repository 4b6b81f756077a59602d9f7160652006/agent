{-# LANGUAGE TemplateHaskell #-}
module Test.Agent.Data.Log where

import           Agent.Data.Log (Log (..), Event (..), Value (..))
import qualified Agent.Data.Log as Log
import qualified Agent.Data.MatrixClock as MatrixClock

import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO (..))
import qualified Control.Monad.ST as ST

import           Data.Foldable (foldlM)
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Tim as Tim

import           Hedgehog hiding (Action)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Prelude hiding (log)

import           System.IO (IO)
import qualified System.Random.MWC as Random
import qualified System.Random.MWC.Distributions as Random

import qualified Test.Agent.Gen as Gen

data Action =
    Append Char
  | Replicate Char Char
  | ReplicateAll Char
    deriving (Eq, Show)

action :: [Char] -> Gen Action
action processes =
  Gen.frequency [
      (5, Append <$> Gen.element processes)
    , (5, Replicate <$> Gen.element processes <*> Gen.element processes)
    , (1, ReplicateAll <$> Gen.element processes)
    ]

prop_simulation :: Property
prop_simulation =
  property $ do
    processes <- forAll $ Gen.filter (not . List.null) Gen.processes
    actions <- forAll $ Gen.list (Range.linear 0 100) (action processes)
    state <- foldlM (\state a -> case a of
      Append process -> do
        v <- forAll Gen.value
        pure $ Map.adjust (Log.append process v) process state
      Replicate source target -> do
        let incoming = fromMaybe (Log.new source) $ Map.lookup source state
        pure $ Map.adjust (\log -> Log.merge target log incoming) target state
      ReplicateAll process -> do
        let incoming = fromMaybe (Log.new process) $ Map.lookup process state
        pure $ foldl (\acc target -> Map.adjust (\log -> Log.merge target log incoming) target acc) state (Map.keys state)
      ) (Map.fromList $ (\p -> (p, Log.new p)) <$> processes) (join [actions, ReplicateAll <$> processes])
    assert $ case Map.elems state of
      [] ->
        True
      (x:xs) ->
        fst $ foldl (\(result, previous) el ->
          (result && Log.length previous == Log.length el && Log.total previous == Log.total previous, el)) (True, x) xs

prop_length_total :: Property
prop_length_total =
  property $ do
    process <- forAll Gen.process
    values <- forAll $ Gen.list (Range.linear 0 100) $ Gen.double (Range.linearFrac 0 1)
    let
      final = foldl (\acc v -> Log.append process (Value v) acc) (Log.new process) values
    Log.length final === length values
    Log.total final === sum (map (\(i, mi) -> i * mi) $ zip [1..] values)

prop_sift_against_sorting_model :: Property
prop_sift_against_sorting_model =
  property $ do
    processes <- forAll $ Gen.filter (not . List.null) Gen.processes
    a <- fmap (Vector.fromList . List.sort) $ forAll $ Gen.events processes
    b <- fmap (Vector.fromList . List.sort) $ forAll $ Gen.events processes
    Log.sift a b ===
      (Vector.uniq $ ST.runST $ do
          v <- Vector.thaw $ a Vector.++ b
          Tim.sort v
          Vector.freeze v)

prop_order_eq :: Property
prop_order_eq =
  property $ do
    processes <- forAll $ Gen.filter (not . List.null) Gen.processes
    a <- forAll $ Gen.event processes
    b <- forAll $ Gen.event processes
    annotateShow $ compare a b
    annotateShow $ a == b
    assert $ (compare a b == EQ && a == b) || (compare a b /= EQ && a /= b)

prop_order_eq_self :: Property
prop_order_eq_self =
  property $ do
    processes <- forAll $ Gen.filter (not . List.null) Gen.processes
    a <- forAll $ Gen.event processes
    annotateShow $ compare a a
    annotateShow $ a == a
    assert $ compare a a == EQ && a == a

prop_total_order :: Property
prop_total_order =
  property $ do
    processes <- forAll $ Gen.filter (not . List.null) Gen.processes
    events <- forAll $ Gen.events processes
    shuffled <- liftIO . Random.withSystemRandom $ \gen ->
      (Random.uniformShuffle (Vector.fromList events) gen) :: IO (Vector (Event Char))
    List.sort events === List.sort (Vector.toList shuffled)

prop_by_example :: Property
prop_by_example =
  property $ do
    let
      logA0 :: Log Char
      logA0 = Log.new 'a'

      logB0 :: Log Char
      logB0 = Log.new 'b'

      logA1 = Log.append 'a' (Value 0.1) logA0

      logB1 = Log.append 'b' (Value 0.2) logB0

      logA2 = Log.append 'a' (Value 0.3) logA1

      logB2 = Log.merge 'b' logB1 logA2

      logA3 = Log.merge 'a' logA2 logB2

      logA4 = Log.append 'a' (Value 0.4) logA3

      deltaA = Log.delta 'b' logA4

      deltaB = Log.delta 'a' logB2

    deltaA ===
      Log (Log.clockOf logA4) (Vector.fromList [Event (MatrixClock.get 'a' . Log.clockOf $ logA4) 'a' (Value 0.4)])

    deltaB ===
      Log (Log.clockOf logB2) (Vector.fromList [Event (MatrixClock.get 'b' . Log.clockOf $ logB1) 'b' (Value 0.2)])


tests :: IO Bool
tests =
  checkParallel $$(discover)
