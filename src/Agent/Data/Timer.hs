{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Agent.Data.Timer (
    Duration (..)
  , Timer (..)
  , new
  , after
  , expired
  , expiredAt
  ) where

import           Control.Monad.IO.Class (MonadIO (..))

import           Data.Binary (Binary)
import           Data.Time (UTCTime)
import qualified Data.Time as Time

import           GHC.Generics (Generic)

newtype Timer =
  Timer {
      getTimerEnd :: UTCTime
    } deriving (Eq, Ord, Show)

newtype Duration =
  Duration {
      seconds :: Int
    } deriving (Enum, Bounded, Eq, Num, Ord, Show, Generic)

instance Binary Duration where

new :: MonadIO m => Duration -> m Timer
new duration =
  liftIO $
    Timer . Time.addUTCTime (fromIntegral $ seconds duration) <$> Time.getCurrentTime

after :: Duration -> Timer -> Timer
after duration =
  Timer . Time.addUTCTime (fromIntegral $ seconds duration) . getTimerEnd

expired :: MonadIO m => Timer -> m Bool
expired timer =
  liftIO $
    flip fmap Time.getCurrentTime $ \now ->
      expiredAt now timer

expiredAt :: UTCTime -> Timer -> Bool
expiredAt time timer =
  time >= getTimerEnd timer
