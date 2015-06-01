module Data.IntervalMap where

import Prelude hiding (lookup)

#ifdef KERNEL
import qualified Data.Map.Base as M
#else
import qualified Data.Map as M
#endif
import Data.Function
import Data.Maybe

newtype IntervalMap k a = IntervalMap (M.Map k (Maybe a))
    deriving Show

empty :: IntervalMap k a
empty = IntervalMap M.empty

singleton :: (Bounded k, Ord k) => (k, k) -> a -> IntervalMap k a
singleton (min, max) a
    | max == maxBound = IntervalMap $ M.singleton min (Just a)
    | otherwise = IntervalMap $ M.fromList [(min, Just a), (max, Nothing)]

-- | Insert a new region. It's an error to insert a new region that overlaps with another
-- but we don't check for that here. Use lookup or inInterval to check for overlap
insert :: (Ord k, Bounded k) => (k, k) -> a -> IntervalMap k a -> IntervalMap k a
insert (min, max) a im@(IntervalMap m)
    | min == max = im
    | otherwise = IntervalMap
                  $ M.insert min (Just a)
                  $ (if max == maxBound then id else insertPrevious)
                  $ m
    where insertPrevious = case M.lookup max m of
                             Nothing ->
                                 case M.lookupLT max m of
                                   Nothing -> M.insert max Nothing
                                   Just (_, a) -> M.insert max a
                             Just _ -> id

-- | Insert with overlap checking. The first function supplied takes in a function that splits an element in the given range into two new ones. One for before the newly inserted range, and one for after
insert' :: (Ord k, Bounded k) => ((k, k) -> a -> (a, a)) -> (k, k) -> a -> IntervalMap k a -> IntervalMap k a
insert' splitOverlap (min, max) a im@(IntervalMap m) = go
    where go = case lookup min im of
                 Nothing -> goMax
                 Just (overlapStart, k) ->
                     let (before, _) = splitOverlap (overlapStart, overlapEnd) k
                         overlapEnd = case M.lookupGT max m of
                                        Just (end, _) -> end
                                        Nothing -> maxBound
                     in insert (overlapStart, min) before $
                        goMax
          goMax = case lookup max im of
                    Nothing -> insert (min, max) a im
                    Just (overlapStart, k) ->
                        let (_, after) = splitOverlap (overlapStart, overlapEnd) k
                            overlapEnd = case M.lookupGT max m of
                                           Just (end, _) -> end
                                           Nothing -> maxBound
                        in insert (max, overlapEnd) after $
                           insert (min, max) a im

lookup :: (Bounded k, Ord k) => k -> IntervalMap k a -> Maybe (k, a)
lookup k (IntervalMap m) = case M.lookup k m of
                             Just (Just a) -> Just (k, a)
                             Just Nothing -> Nothing
                             Nothing ->
                               case M.lookupLT k m of
                                 Just (k, Just v) -> Just (k, v)
                                 Just (_, Nothing) -> Nothing
                                 Nothing -> Nothing

fromList :: (Bounded k, Ord k) => [((k, k), a)] -> IntervalMap k a
fromList = foldr (uncurry insert) empty

assocs :: (Bounded k, Ord k) => IntervalMap k a -> [((k, k), a)]
assocs (IntervalMap m) = let mapAssocs = M.assocs m
                             intervalAssocs = zipWith (\(min, x) (max, _) -> ((min, max), x)) ((minBound, Nothing):mapAssocs) (mapAssocs ++ [(maxBound, Nothing)])
                         in mapMaybe (\((min, max), j) ->
                                          case j of
                                            Nothing -> Nothing
                                            Just j -> Just ((min, max), j)) intervalAssocs
