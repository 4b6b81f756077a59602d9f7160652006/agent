{-# LANGUAGE TemplateHaskell #-}
module Test.Agent.Data.VectorClock where

import           Agent.Data.VectorClock (VectorClock (..))
import qualified Agent.Data.VectorClock as VectorClock

import           Data.Bits (xor)

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           System.IO (IO)

import qualified Test.Agent.Gen as Gen


empty :: VectorClock Char
empty =
  VectorClock.new

prop_symmetric_equals :: Property
prop_symmetric_equals =
  property $ do
    a <- forAll Gen.vectorClock
    b <- forAll Gen.vectorClock
    assert $ (a == b) `xor` (a /= b)

prop_reflexive_equals :: Property
prop_reflexive_equals =
  property $ do
    a <- forAll Gen.vectorClock
    assert $ a == a

prop_everything_descends_empty :: Property
prop_everything_descends_empty =
  property $ do
    v <- forAll Gen.vectorClock
    assert $ v `VectorClock.descends` empty

prop_everything_except_empty_dominates_empty :: Property
prop_everything_except_empty_dominates_empty =
  property $ do
    v <- forAll $ Gen.filter (/= empty) $ Gen.vectorClock
    assert $ v `VectorClock.dominates` empty

prop_merge_descends :: Property
prop_merge_descends =
  property $ do
    a <- forAll Gen.vectorClock
    b <- forAll Gen.vectorClock
    assert $ VectorClock.merge a b `VectorClock.descends` a
    assert $ VectorClock.merge a b `VectorClock.descends` b

prop_merge_tick_dominates :: Property
prop_merge_tick_dominates =
  property $ do
    a <- forAll Gen.vectorClock
    b <- forAll Gen.vectorClock
    p <- forAll Gen.process
    let
      ab = VectorClock.merge a b
      abc = VectorClock.tick p ab
    annotateShow ab
    annotateShow abc
    assert $ abc `VectorClock.dominates` a
    assert $ abc `VectorClock.dominates` b
    assert $ abc `VectorClock.dominates` ab

prop_by_example :: Property
prop_by_example =
  property $ do
    assert $ empty `VectorClock.descends` empty
    assert $ VectorClock.fromList [('a', 3), ('b', 2), ('c', 1)] `VectorClock.descends` VectorClock.fromList [('a', 3), ('b', 2), ('c', 1)]
    assert $ VectorClock.fromList [('a', 3), ('b', 2), ('c', 1)] `VectorClock.descends` VectorClock.fromList [('a', 2), ('b', 2), ('c', 1)]
    assert $ VectorClock.fromList [('a', 3), ('b', 2), ('c', 1)] `VectorClock.dominates` VectorClock.fromList [('a', 2), ('b', 2), ('c', 1)]
    assert . not $ VectorClock.fromList [('a', 1)] `VectorClock.dominates` VectorClock.fromList [('b', 1)]
    assert . not $ VectorClock.fromList [('b', 1)] `VectorClock.dominates` VectorClock.fromList [('a', 1)]
    assert . not $ VectorClock.fromList [('a', 1)] `VectorClock.descends` VectorClock.fromList [('b', 1)]
    assert . not $ VectorClock.fromList [('b', 1)] `VectorClock.descends` VectorClock.fromList [('a', 1)]

tests :: IO Bool
tests =
  checkParallel $$(discover)
