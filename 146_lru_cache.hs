module LRUCache where

import qualified Data.Map.Strict as Map
import Data.Maybe

data LRUCache k v = LRUCache
  { capacity :: Int
  , order :: [k]
  , store :: Map.Map k v
  } deriving (Show)

newLRUCache :: Int -> LRUCache k v
newLRUCache cap = LRUCache
  { capacity = cap
  , order = []
  , store = Map.empty
  }

get :: Ord k => k -> LRUCache k v -> (Maybe v, LRUCache k v)
get key cache = case Map.lookup key (store cache) of
  Nothing -> (Nothing, cache)
  Just value -> (Just value, moveToFront key cache)

put :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
put key value cache
  | Map.member key (store cache) = updateExisting key value cache
  | length (order cache) < capacity cache = insertNew key value cache
  | otherwise = evictAndInsert key value cache

moveToFront :: Eq k => k -> LRUCache k v -> LRUCache k v
moveToFront key cache = cache { order = key : filter (/= key) (order cache) }

updateExisting :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
updateExisting key value cache = moveToFront key cache { store = Map.insert key value (store cache) }

insertNew :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
insertNew key value cache = LRUCache
  { capacity = capacity cache
  , order = key : order cache
  , store = Map.insert key value (store cache)
  }

evictAndInsert :: Ord k => k -> v -> LRUCache k v -> LRUCache k v
evictAndInsert key value cache = case order cache of
  [] -> insertNew key value cache
  _ : rest -> LRUCache
    { capacity = capacity cache
    , order = key : init rest
    , store = Map.insert key value (Map.delete (last rest) (store cache))
    }