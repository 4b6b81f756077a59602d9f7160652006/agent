{-# LANGUAGE TemplateHaskell #-}
module Test.Agent.Data.Clock where

import qualified Agent.Data.Clock as Clock

import           Hedgehog

import           System.IO (IO)

import qualified Test.Agent.Gen as Gen


prop_tick_order :: Property
prop_tick_order =
  property $ do
    c <- forAll Gen.clock
    assert $ Clock.tick c > c

tests :: IO Bool
tests =
  checkParallel $$(discover)
