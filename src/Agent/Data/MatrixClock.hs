{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Agent.Data.MatrixClock (
    MatrixClock (..)
  , new
  , withProcess
  , tick
  , set
  , get
  , setv
  , getv
  , merge
  , maximum
  , refresh
  , fromList
  ) where

import           Agent.Data.Clock (Clock)
import qualified Agent.Data.Clock as Clock
import           Agent.Data.VectorClock (VectorClock)
import qualified Agent.Data.VectorClock as VectorClock

import           Data.Binary (Binary)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge

import           GHC.Generics (Generic)

import           Prelude hiding (maximum)

-- |
-- A 'MatrixClock' keeps track of the 'VectorClock' that each
-- process @a@ has acknowledged, i.e. the owner of a 'VectorClock'
-- knows the times they have received updates from each other process,
-- the owner of a 'MatrixClock' _also_ knows the times that each other
-- process has received updates for. This allows a _cut_ to be
-- determined that allows for incremental updates.
newtype MatrixClock a =
  MatrixClock {
      toMap :: Map a (VectorClock a)
    } deriving (Eq, Show, Generic)

instance Binary a => Binary (MatrixClock a) where

new :: MatrixClock a
new =
  MatrixClock Map.empty

-- |
-- Update a specific processes 'VectorClock'.
--
withProcess :: Ord a => a -> MatrixClock a -> (VectorClock a -> VectorClock a) -> MatrixClock a
withProcess a clock f =
  MatrixClock . Map.insert a (f $ getv a clock) $ toMap clock

-- |
-- Increment process @a@'s time, if @a@ doesn't exist yet, it
-- will default to @Clock 0@ and be incremented.
tick :: Ord a => a -> MatrixClock a -> MatrixClock a
tick a clock =
  withProcess a clock $
    VectorClock.tick a

set :: Ord a => a -> Clock -> MatrixClock a -> MatrixClock a
set a c clock =
  withProcess a clock $
    VectorClock.set a c

get :: Ord a => a -> MatrixClock a -> Clock
get a m =
  VectorClock.get a $ getv a m

setv :: Ord a => a -> VectorClock a -> MatrixClock a -> MatrixClock a
setv a c (MatrixClock clock) =
  MatrixClock $ Map.insert a c clock

getv :: Ord a => a -> MatrixClock a -> VectorClock a
getv a (MatrixClock clock) =
  Map.findWithDefault VectorClock.new a clock

-- |
-- A pair of 'MatrixClock' can be merged by taking the pairwise maximum of
-- each process's 'VectorClock'.
merge :: Ord a => MatrixClock a -> MatrixClock a -> MatrixClock a
merge a b =
  MatrixClock $
    Merge.merge
      Merge.preserveMissing
      Merge.preserveMissing
      (Merge.zipWithMatched (const VectorClock.merge))
      (toMap a)
      (toMap b)

-- |
-- The maximum 'Clock' of a 'MatrixClock' is useful to determine the
-- next Lamport 'Clock' time for a local can be merged by taking the
-- pairwise maximum of each entry.
maximum :: Ord a => MatrixClock a -> Clock
maximum (MatrixClock clock) =
  case VectorClock.maximum <$> Map.elems clock of
    [] ->
      Clock.new
    (x:xs) ->
      List.foldl' max x xs

-- |
-- Refresh the 'VectorClock' for process @a@ based on the state
-- of the 'MatrixClock'. Refreshing means merging each other
-- processes 'VectorClock' into @a@'s 'VectorClock' and then
-- setting the local 'Clock' to the global @maximum@ time, and
-- incrementing.
refresh :: Ord a => a -> MatrixClock a -> MatrixClock a
refresh a clock =
  tick a . (\c -> set a (maximum c) c) . withProcess a clock $ \mine ->
    List.foldl' VectorClock.merge mine (Map.elems $ toMap clock)

fromList :: Ord a => [(a, VectorClock a)] -> MatrixClock a
fromList =
  MatrixClock . Map.fromList
