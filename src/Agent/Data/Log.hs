{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Agent.Data.Log (
    Value (..)
  , Event (..)
  , Log (..)
  , new
  , null
  , clockOf
  , append
  , merge
  , length
  , total
  , delta
  , sift
  ) where

import           Agent.Data.Clock (Clock)
import qualified Agent.Data.VectorClock as VectorClock
import           Agent.Data.MatrixClock (MatrixClock)
import qualified Agent.Data.MatrixClock as MatrixClock

import qualified Control.Monad.ST as ST

import           Data.Binary (Binary)
import           Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MutableVector
import           Data.Vector.Binary ()

import           GHC.Generics (Generic)

import           Prelude hiding (null, length)


newtype Value =
  Value {
      getValue :: Double
    } deriving (Eq, Ord, Show, Generic)

instance Binary Value where

data Event a =
  Event {
      eventClock :: !Clock
    , eventProcessId :: !a
    , eventValue :: !Value
    } deriving (Eq, Show, Generic)

instance Ord a => Ord (Event a) where
  compare a b =
    compare
      (eventClock a, eventProcessId a, eventValue a)
      (eventClock b, eventProcessId b, eventValue b)

instance Binary a => Binary (Event a) where

-- |
-- The 'Log' is basically a pair of a processes 'MatrixClock'
-- and an ordered sequence of events. The ordered sequence
-- basically acts similar to a priority-queue/heap but this
-- implementation is a bit more direct because it gets called
-- a lot.
data Log a =
  Log {
      logClock :: !(MatrixClock a)
    , logEvents :: !(Vector (Event a))
    } deriving (Eq, Show, Generic)

instance Binary a => Binary (Log a) where

new :: Ord a => a -> Log a
new a =
  Log (MatrixClock.tick a $ MatrixClock.new) Vector.empty

null :: Log a -> Bool
null =
 Vector.null . logEvents

clockOf :: Log a -> MatrixClock a
clockOf =
  logClock

-- |
-- The number of messages in the log.
length :: (Log a) -> Int
length =
  Vector.length . logEvents

-- |
-- The total value of the log:
--
-- @
-- sum (imap (\i mi -> i * mi) m)
-- @
total :: (Log a) -> Double
total =
  Vector.sum .
    Vector.imap (\i -> (*) (fromIntegral $ i + 1) . getValue . eventValue) .
      logEvents

-- |
-- Local append a 'Value' for the process @a@. The internal
-- 'MatrixClock' ensures this can be appended in order.
append :: Ord a => a -> Value -> Log a -> Log a
append a v (Log clock events) =
  let
    updated = MatrixClock.tick a clock
    now = MatrixClock.get a updated
    event = Event now a v
  in
    Log updated (Vector.snoc events event)

-- |
-- Merge two 'Log' from the perspective of process @a@. The
-- resultant log will contain all events (sorted) from both logs and
-- the internal 'MatrixClock' will be updated to reflect the new time
-- horizons.
merge :: Ord a => a -> Log a -> Log a -> Log a
merge a (Log cx x) (Log cy y) =
  Log (MatrixClock.refresh a $ MatrixClock.merge cx cy) $ sift x y

-- |
-- Calculates the /delta/ that is a subset of log @l@ that has not
-- been acknowledged by process @a@.
--
delta :: Ord a => a -> (Log a) -> (Log a)
delta a l =
  let
    clock = logClock l
    vector = MatrixClock.getv a clock
  in
    -- This is basically an optimised @dominates@ filter for a single
    -- clock.  Could use @dominates@ on a singleton 'VectorClock' but
    -- it is nowhere near as efficient for this special case.
    Log clock $ Vector.filter (\e ->
      let seen = VectorClock.get (eventProcessId e) vector
       in eventClock e > seen && not (a == eventProcessId e)) $ logEvents l

-- |
-- A sorted merge of two vectors. Both input vectors must be __sorted__.
--
-- This is a more efficient version of @uniq . sort $ x ++ y@.
sift :: Ord a => Vector a -> Vector a -> Vector a
sift x y = ST.runST $ do
  v <- MutableVector.new (Vector.length x + Vector.length y)
  let
    go !index !xindex !yindex =
      case (x Vector.!? xindex, y Vector.!? yindex) of
        (Just vx, Just vy) ->
          if vx == vy then do
            MutableVector.write v index vx
            go (index + 1) (xindex + 1) (yindex + 1)
          else if vx < vy then do
            MutableVector.write v index vx
            go (index + 1) (xindex + 1) yindex
          else do
            MutableVector.write v index vy
            go (index + 1) xindex (yindex + 1)
        (Just vx, Nothing) -> do
          MutableVector.write v index vx
          go (index + 1) (xindex + 1) yindex
        (Nothing, Just vy) -> do
          MutableVector.write v index vy
          go (index + 1) xindex (yindex + 1)
        (Nothing, Nothing) ->
          Vector.freeze $ MutableVector.slice 0 index v
  go 0 0 0
