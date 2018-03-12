{-# LANGUAGE DeriveGeneric #-}
module Agent.Data.VectorClock (
    VectorClock (..)
  , new
  , singleton
  , fromList
  , tick
  , set
  , get
  , merge
  , maximum
  , descends
  , dominates
  ) where

import           Agent.Data.Clock (Clock)
import qualified Agent.Data.Clock as Clock

import           Data.Binary (Binary)
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Map.Merge.Strict as Merge

import           GHC.Generics (Generic)

import           Prelude hiding (maximum)


-- |
-- A 'VectorClock' keeps track of the logical 'Clock' on each process @a@.
newtype VectorClock a =
  VectorClock {
      toMap :: Map a Clock
    } deriving (Eq, Show, Generic)

instance Binary a => Binary (VectorClock a) where

new :: VectorClock a
new =
  VectorClock Map.empty

singleton :: a -> Clock -> VectorClock a
singleton a =
  VectorClock . Map.singleton a

fromList :: Ord a => [(a, Clock)] -> VectorClock a
fromList =
  VectorClock . Map.fromList

tick :: Ord a => a -> VectorClock a -> VectorClock a
tick a =
  VectorClock . Map.insertWith (const Clock.tick) a (Clock.tick Clock.new) . toMap

set :: Ord a => a -> Clock -> VectorClock a -> VectorClock a
set a clock =
  VectorClock . Map.insert a clock . toMap

get :: Ord a => a -> VectorClock a -> Clock
get a =
  Map.findWithDefault Clock.new a . toMap

-- |
-- A pair of 'VectorClock' can be merged by taking the pairwise maximum of
-- each entry.
merge :: Ord a => VectorClock a -> VectorClock a -> VectorClock a
merge a b =
  VectorClock $
    Merge.merge
      Merge.preserveMissing
      Merge.preserveMissing
      (Merge.zipWithMatched (const max))
      (toMap a)
      (toMap b)

-- |
-- The maximum 'Clock' of a 'VectorClock' is useful to determine the next
-- Lamport 'Clock' time for a loca can be merged by taking the pairwise
-- maximum of each entry.
maximum :: Ord a => VectorClock a -> Clock
maximum (VectorClock clock) =
  case Map.elems clock of
    [] ->
      Clock.new
    (x:xs) ->
      List.foldl' max x xs

-- |
-- The @a@ `descends` @b@ if @a@ summarises all events @b@ does.
-- In terms of ordering this can be thought of as @a >= b@.
descends :: Ord a => VectorClock a -> VectorClock a -> Bool
descends a b =
  and . flip fmap (Map.assocs . toMap $ b) $ \(k, vb) ->
    case Map.lookup k (toMap a) of
      Nothing ->
        False
      Just va ->
        va >= vb

-- |
-- The @a@ `dominates` @b@ if @a@ summarises all events @b@ does
-- plus contains _additional_ events that @b@ does not capture.
-- In terms of ordering this can be thought of as @a > b@.
dominates :: Ord a => VectorClock a -> VectorClock a -> Bool
dominates a b =
  descends a b && not (descends b a)
