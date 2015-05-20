{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.PQueue.Min
-- Copyright   :  (c) Louis Wasserman 2010
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- General purpose priority queue, supporting extract-minimum operations.
--
-- An amortized running time is given for each operation, with /n/ referring
-- to the length of the sequence and /k/ being the integral index used by
-- some operations.  These bounds hold even in a persistent (shared) setting.
--
-- This implementation is based on a binomial heap augmented with a global root.
-- The spine of the heap is maintained lazily.  To force the spine of the heap,
-- use 'seqSpine'.
--
-- This implementation does not guarantee stable behavior.
-- 
-- This implementation offers a number of methods of the form @xxxU@, where @U@ stands for
-- unordered.  No guarantees whatsoever are made on the execution or traversal order of
-- these functions.
-----------------------------------------------------------------------------
module Data.PQueue.Min (
	MinQueue,
	-- * Basic operations
	empty,
	null,
	size, 
	-- * Query operations
	findMin,
	getMin,
	deleteMin,
	deleteFindMin,
	minView,
	-- * Construction operations
	singleton,
	insert,
	union,
	unions,
	-- * Subsets
	-- ** Extracting subsets
	(!!),
	take,
	drop,
	splitAt,
	-- ** Predicates
	takeWhile,
	dropWhile,
	span,
	break,
	-- * Filter/Map
	filter,
	partition,
	mapMaybe,
	mapEither,
	-- * Fold\/Functor\/Traversable variations
	map,
	foldrAsc,
	foldlAsc,
	foldrDesc,
	foldlDesc,
	-- * List operations
	toList,
	toAscList,
	toDescList,
	fromList,
	fromAscList,
	fromDescList,
	-- * Unordered operations
	mapU,
	foldrU,
	foldlU,
	elemsU,
	toListU,
	-- * Miscellaneous operations
	keysQueue,
	seqSpine) where

import Prelude hiding (null, foldr, foldl, take, drop, takeWhile, dropWhile, splitAt, span, break, (!!), filter, map)

import Control.Applicative (Applicative(..), (<$>))
import Control.Applicative.Identity

import Data.Monoid
import Data.Maybe hiding (mapMaybe)
import Data.Foldable hiding (toList)
import Data.Traversable

import qualified Data.List as List

import Data.PQueue.Internals

#ifdef __GLASGOW_HASKELL__
import GHC.Exts (build)
import Text.Read (Lexeme(Ident), lexP, parens, prec,
	readPrec, readListPrec, readListPrecDefault)
import Data.Data
#else
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build f = f (:) []
#endif

-- instance 

instance (Ord a, Show a) => Show (MinQueue a) where
	showsPrec p xs = showParen (p > 10) $
		showString "fromAscList " . shows (toAscList xs)

instance Read a => Read (MinQueue a) where
#ifdef __GLASGOW_HASKELL__
	readPrec = parens $ prec 10 $ do
		Ident "fromAscList" <- lexP
		xs <- readPrec
		return (fromAscList xs)

	readListPrec = readListPrecDefault
#else
	readsPrec p = readParen (p > 10) $ \ r -> do
		("fromAscList",s) <- lex r
		(xs,t) <- reads s
		return (fromAscList xs,t)
#endif

instance Ord a => Monoid (MinQueue a) where
	mempty = empty
	mappend = union
	mconcat = unions

-- | /O(1)/.  Returns the minimum element.  Throws an error on an empty queue.
findMin :: MinQueue a -> a
findMin = fromMaybe (error "Error: findMin called on empty queue") . getMin

-- | /O(log n)/.  Deletes the minimum element.  If the queue is empty, does nothing.
deleteMin :: Ord a => MinQueue a -> MinQueue a
deleteMin q = case minView q of
	Nothing		-> empty
	Just (_, q')	-> q'

-- | /O(log n)/.  Extracts the minimum element.  Throws an error on an empty queue.
deleteFindMin :: Ord a => MinQueue a -> (a, MinQueue a)
deleteFindMin = fromMaybe (error "Error: deleteFindMin called on empty queue") . minView

-- | Takes the union of a list of priority queues.  Equivalent to @'foldl' 'union' 'empty'@.
unions :: Ord a => [MinQueue a] -> MinQueue a
unions = foldl union empty

-- | /O(k log n)/.  Index (subscript) operator, starting from 0.  @queue !! k@ returns the @(k+1)@th smallest 
-- element in the queue.  Equivalent to @toAscList queue !! k@.
(!!) :: Ord a => MinQueue a -> Int -> a
q !! n	| n >= size q
		= error "Data.PQueue.Min.!!: index too large"
q !! n = (List.!!) (toAscList q) n

{-# INLINE takeWhile #-}
-- | 'takeWhile', applied to a predicate @p@ and a queue @queue@, returns the
-- longest prefix (possibly empty) of @queue@ of elements that satisfy @p@.
takeWhile :: Ord a => (a -> Bool) -> MinQueue a -> [a]
takeWhile p = foldWhileFB p . toAscList

{-# INLINE foldWhileFB #-}
-- | Equivalent to Data.List.takeWhile, but is a better producer.
foldWhileFB :: (a -> Bool) -> [a] -> [a]
foldWhileFB p xs = build (\ c nil -> let 
	consWhile x xs
		| p x		= x `c` xs
		| otherwise	= nil
	in foldr consWhile nil xs)

-- | 'dropWhile' @p queue@ returns the queue remaining after 'takeWhile' @p queue@.
dropWhile :: Ord a => (a -> Bool) -> MinQueue a -> MinQueue a
dropWhile p = drop' where
	drop' q = case minView q of
	  Just (x, q')
		| p x	-> drop' q'
	  _		-> q

-- | 'span', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- satisfy @p@ and second element is the remainder of the queue.
span :: Ord a => (a -> Bool) -> MinQueue a -> ([a], MinQueue a)
span p queue = case minView queue of
	Just (x, q') 
		| p x	-> let (ys, q'') = span p q' in (x:ys, q'')
	_		-> ([], queue)

-- | 'break', applied to a predicate @p@ and a queue @queue@, returns a tuple where
-- first element is longest prefix (possibly empty) of @queue@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the queue.
break :: Ord a => (a -> Bool) -> MinQueue a -> ([a], MinQueue a)
break p = span (not . p)

{-# INLINE take #-}
-- | /O(k log n)/. 'take' @k@, applied to a queue @queue@, returns a list of the smallest @k@ elements of @queue@,
-- or all elements of @queue@ itself if @k >= 'size' queue@.
take :: Ord a => Int -> MinQueue a -> [a]
take n = List.take n . toAscList

-- | /O(k log n)/.  'drop' @k@, applied to a queue @queue@, returns @queue@ with the smallest @k@ elements deleted,
-- or an empty queue if @k >= size 'queue'@.
drop :: Ord a => Int -> MinQueue a -> MinQueue a
drop n queue = n `seq` case minView queue of
	Just (_, queue')
	  | n > 0	-> drop (n-1) queue'
	_		-> queue

-- | /O(k log n)/.  Equivalent to @('take' k queue, 'drop' k queue)@.
splitAt :: Ord a => Int -> MinQueue a -> ([a], MinQueue a)
splitAt n queue = n `seq` case minView queue of
	Just (x, queue')
	  | n > 0	-> let (xs, queue'') = splitAt (n-1) queue' in (x:xs, queue'')
	_		-> ([], queue)

-- | /O(n)/.  Returns the queue with all elements not satisfying @p@ removed.
filter :: Ord a => (a -> Bool) -> MinQueue a -> MinQueue a
filter p = mapMaybe (\ x -> if p x then Just x else Nothing)

-- | /O(n)/.  Returns a pair where the first queue contains all elements satisfying @p@, and the second queue
-- contains all elements not satisfying @p@.
partition :: Ord a => (a -> Bool) -> MinQueue a -> (MinQueue a, MinQueue a)
partition p = mapEither (\ x -> if p x then Left x else Right x)

-- | /O(n)/.  Creates a new priority queue containing the images of the elements of this queue.
-- Equivalent to @'fromList' . 'Data.List.map' f . toList@.
map :: Ord b => (a -> b) -> MinQueue a -> MinQueue b
map f = foldrU (insert . f) empty

{-# INLINE toAscList #-}
-- | /O(n log n)/.  Extracts the elements of the priority queue in ascending order.
toAscList :: Ord a => MinQueue a -> [a]
toAscList queue = build (\ c nil -> foldrAsc c nil queue)

{-# INLINE toDescList #-}
-- | /O(n log n)/.  Extracts the elements of the priority queue in descending order.
toDescList :: Ord a => MinQueue a -> [a]
toDescList queue = build (\ c nil -> foldrDesc c nil queue)

{-# INLINE toList #-}
-- | /O(n)/.  Returns the elements of the priority queue in ascending order.  Equivalent to 'toAscList'.
-- 
-- If the order of the elements is irrelevant, consider using 'toListU'.
toList :: Ord a => MinQueue a -> [a]
toList = toAscList

{-# RULES
	"toAscList" forall q . toAscList q = build (\ c nil -> foldrAsc c nil q);
		-- inlining doesn't seem to be working out =/
	"toDescList" forall q . toDescList q = build (\ c nil -> foldrDesc c nil q);
	#-}

-- | /O(n log n)/.  Performs a right-fold on the elements of a priority queue in descending order.
-- @foldrDesc f z q == foldlAsc (flip f) z q@.
foldrDesc :: Ord a => (a -> b -> b) -> b -> MinQueue a -> b
foldrDesc = foldlAsc . flip

-- | /O(n log n)/.  Performs a left-fold on the elements of a priority queue in descending order.
-- @foldlDesc f z q == foldrAsc (flip f) z q@.
foldlDesc :: Ord a => (b -> a -> b) -> b -> MinQueue a -> b
foldlDesc = foldrAsc . flip

{-# INLINE fromList #-}
-- | /O(n)/.  Constructs a priority queue from an unordered list.
fromList :: Ord a => [a] -> MinQueue a
fromList = foldr insert empty

{-# RULES
	"fromList" fromList = foldr insert empty;
	"fromAscList" fromAscList = foldr insertMinQ empty;
	#-}

{-# INLINE fromAscList #-}
-- | /O(n)/.  Constructs a priority queue from an ascending list.  /Warning/: Does not check the precondition.
fromAscList :: [a] -> MinQueue a
fromAscList = foldr insertMinQ empty

-- | /O(n)/.  Constructs a priority queue from an descending list.  /Warning/: Does not check the precondition.
fromDescList :: [a] -> MinQueue a
fromDescList = foldl' (flip insertMinQ) empty

-- | Maps a function over the elements of the queue, ignoring order.  This function is only safe if the function is monotonic.
-- This function /does not/ check the precondition.
mapU :: (a -> b) -> MinQueue a -> MinQueue b
mapU = mapMonotonic

{-# INLINE elemsU #-}
-- | Equivalent to 'toListU'.
elemsU :: MinQueue a -> [a]
elemsU = toListU

-- | Returns the elements of the queue, in no particular order.
toListU :: MinQueue a -> [a]
toListU q = build (\ c n -> foldrU c n q)

{-# RULES
	"foldr/toListU" forall f z q . foldr f z (toListU q) = foldrU f z q;
	"foldl/toListU" forall f z q . foldl f z (toListU q) = foldlU f z q;
	#-}
