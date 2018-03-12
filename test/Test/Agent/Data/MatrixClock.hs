{-# LANGUAGE TemplateHaskell #-}
module Test.Agent.Data.MatrixClock where

import           Agent.Data.MatrixClock (MatrixClock (..))
import qualified Agent.Data.MatrixClock as MatrixClock
import qualified Agent.Data.VectorClock as VectorClock

import           Hedgehog

import           System.IO (IO)

import qualified Test.Agent.Gen as Gen


empty :: MatrixClock Char
empty =
  MatrixClock.new

prop_empty_is_merge_identity :: Property
prop_empty_is_merge_identity = do
  property $ do
    clock <- forAll Gen.matrixClock
    MatrixClock.merge clock empty === clock
    MatrixClock.merge empty clock === clock

prop_tick_order :: Property
prop_tick_order = do
  property $ do
    p <- forAll Gen.process
    c <- forAll Gen.matrixClock
    annotateShow $ MatrixClock.get p c
    annotateShow $ MatrixClock.get p (MatrixClock.tick p c)
    assert $ MatrixClock.get p c < MatrixClock.get p (MatrixClock.tick p c)

prop_symmetric_set :: Property
prop_symmetric_set = do
  property $ do
    p <- forAll Gen.process
    m <- forAll Gen.matrixClock
    c <- forAll Gen.clock
    c === MatrixClock.get p (MatrixClock.set p c m)

prop_symmetric_set_vector :: Property
prop_symmetric_set_vector = do
  property $ do
    p <- forAll Gen.process
    m <- forAll Gen.matrixClock
    v <- forAll Gen.vectorClock
    v === MatrixClock.getv p (MatrixClock.setv p v m)

prop_maximum :: Property
prop_maximum = do
  property $ do
    p <- forAll Gen.process
    m <- forAll Gen.matrixClock
    c <- forAll Gen.clock
    assert $ MatrixClock.maximum (MatrixClock.set p c m) >= c

prop_refresh :: Property
prop_refresh = do
  property $ do
    p1 <- forAll Gen.process
    p2 <- forAll Gen.process
    m <- forAll Gen.matrixClock
    v <- forAll Gen.vectorClock
    assert $
      VectorClock.dominates
        (MatrixClock.getv p1 (MatrixClock.refresh p1 (MatrixClock.setv p2 v m)))
        v

prop_by_example :: Property
prop_by_example =
  property $ do
    MatrixClock.merge
      (empty)
      (empty)
        === MatrixClock.fromList []

    MatrixClock.merge
      (MatrixClock.fromList [('a', VectorClock.fromList [('a', 10), ('b', 3)])])
      (MatrixClock.fromList [('b', VectorClock.fromList [('a', 7), ('b', 10)])])
        === MatrixClock.fromList [
                ('a', VectorClock.fromList [('a', 10), ('b', 3)])
              , ('b', VectorClock.fromList [('a', 7), ('b', 10)])
              ]

    MatrixClock.refresh 'a' (MatrixClock.merge
      (MatrixClock.fromList [('a', VectorClock.fromList [('a', 10), ('b', 3)])])
      (MatrixClock.fromList [('b', VectorClock.fromList [('a', 7), ('b', 10)])]))
        === MatrixClock.fromList [
                ('a', VectorClock.fromList [('a', 11), ('b', 10)])
              , ('b', VectorClock.fromList [('a', 7), ('b', 10)])
              ]

    MatrixClock.get 'a' (MatrixClock.refresh 'a' (MatrixClock.merge
      (MatrixClock.fromList [('a', VectorClock.fromList [('a', 10), ('b', 3)])])
      (MatrixClock.fromList [('b', VectorClock.fromList [('a', 7), ('b', 10)])])))
        === 11

tests :: IO Bool
tests =
  checkParallel $$(discover)
