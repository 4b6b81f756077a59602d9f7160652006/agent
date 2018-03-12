{-# LANGUAGE TemplateHaskell #-}
module Test.Agent.Data.Random where

import qualified Agent.Data.Random as Random

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           System.IO (IO)

import qualified Test.Agent.Gen as Gen


prop_deterministic :: Property
prop_deterministic =
  property $ do
    s <- forAll Gen.seed
    a <- Random.new s >>= Random.next
    b <- Random.new s >>= Random.next
    a === b

prop_non_deterministic :: Property
prop_non_deterministic =
  property $ do
    s1 <- forAll Gen.seed
    s2 <- forAll $ Gen.filter (/= s1) Gen.seed
    a <- Random.new s1 >>= Random.next
    b <- Random.new s2 >>= Random.next
    assert $ a /= b

prop_non_deterministic_gen :: Property
prop_non_deterministic_gen =
  property $ do
    s <- forAll Gen.seed
    r <- Random.new s
    a <- Random.next r
    b <- Random.next r
    assert $ a /= b

tests :: IO Bool
tests =
  checkParallel $$(discover)
