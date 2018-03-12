{-# LANGUAGE TemplateHaskell #-}
module Test.Agent.Data.Timer where

import           Agent.Data.Timer (Timer (..))
import qualified Agent.Data.Timer as Timer

import           Control.Monad.IO.Class (MonadIO (..))

import           Hedgehog
import qualified Hedgehog.Gen as Gen

import           System.IO (IO)

import qualified Test.Agent.Gen as Gen


prop_now_expired :: Property
prop_now_expired =
  property $ do
    t <- liftIO $ Timer.new 0
    assert $ Timer.expiredAt (getTimerEnd t) t

prop_after_not_expired :: Property
prop_after_not_expired =
  property $ do
    t <- liftIO $ Timer.new 0
    d <- forAll $ Gen.filter (> 0) Gen.duration
    annotateShow t
    annotateShow $ Timer.after d t
    assert . not $ Timer.expiredAt (getTimerEnd t) (Timer.after d t)

tests :: IO Bool
tests =
  checkParallel $$(discover)
