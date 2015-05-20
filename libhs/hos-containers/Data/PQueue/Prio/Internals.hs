{-# LANGUAGE CPP #-}
module Data.PQueue.Prio.Internals (
	MinPQueue(..),
	BinomForest(..),
	BinomHeap,
	BinomTree(..),
	Zero(..),
	Succ(..),
	LEq,
	empty,
	null,
	size,
	singleton,
	insert,
	union,
	getMin,
	adjustMinWithKey,
	updateMinWithKey,
	minViewWithKey,
	mapWithKey,
	mapKeysMonotonic,
	mapMaybeWithKey,
	mapEitherWithKey,
	foldrWithKey,
	foldlWithKey,
	insertMin,
	foldrWithKeyU,
	foldlWithKeyU,
	traverseWithKeyU,
	seqSpine
	) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Applicative.Identity

import Data.Monoid (Monoid (..))
import Prelude hiding (null)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) f g x y = f (g x y)

first' :: (a -> b) -> (a, c) -> (b, c)
first' f (a, c) = (f a, c)

second' :: (b -> c) -> (a, b) -> (a, c)
second' f (a, b) = (a, f b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

infixr 8 .:

-- | A priority queue where values of type @a@ are annotated with keys of type @k@.
-- The queue supports extracting the element with minimum key.
data MinPQueue k a = Empty | MinPQ {-# UNPACK #-} !Int k a (BinomHeap k a)
#if __GLASGOW_HASKELL__
	deriving (Typeable)
#endif

data BinomForest rk k a = 
	Nil |
	Skip (BinomForest (Succ rk) k a) |
	Cons {-# UNPACK #-} !(BinomTree rk k a) (BinomForest (Succ rk) k a)
type BinomHeap = BinomForest Zero

data BinomTree rk k a = BinomTree k a (rk k a)
data Zero k a = Zero
data Succ rk k a = Succ {-# UNPACK #-} !(BinomTree rk k a) (rk k a)

type LEq a = a -> a -> Bool

instance (Ord k, Eq a) => Eq (MinPQueue k a) where
	MinPQ n1 k1 a1 ts1 == MinPQ n2 k2 a2 ts2 =
		n1 == n2 && k1 == k2 && a1 == a2 && equHeap ts1 ts2
	 where	equHeap ts1 ts2 = case (extract ts1, extract ts2) of
	 		(Yes (Extract k1 a1 _ ts1'), Yes (Extract k2 a2 _ ts2'))
				-> k1 == k2 && a1 == a2 && equHeap ts1' ts2'
			(No, No) -> True
			_	-> False
		extract = extractForest (<=)
	Empty == Empty = True
	_ == _ = False

(<>) :: Monoid m => m -> m -> m
(<>) = mappend
infixr 6 <>

instance (Ord k, Ord a) => Ord (MinPQueue k a) where
	MinPQ n1 k1 a1 ts1 `compare` MinPQ n2 k2 a2 ts2 =
		k1 `compare` k2 <> a1 `compare` a2 <> ts1 `cmpHeap` ts2
	 where	ts1 `cmpHeap` ts2 = case (extract ts1, extract ts2) of
	 		(Yes (Extract k1 a1 _ ts1'), Yes (Extract k2 a2 _ ts2'))
				-> k1 `compare` k2 <> a1 `compare` a2 <> ts1' `cmpHeap` ts2'
			(No, Yes{})	-> LT
			(Yes{}, No)	-> GT
			(No, No)	-> EQ
		extract = extractForest (<=)
	Empty `compare` Empty = EQ
	Empty `compare` MinPQ{} = LT
	MinPQ{} `compare` Empty = GT

-- | /O(1)/.  Returns the empty priority queue.
empty :: MinPQueue k a
empty = Empty

-- | /O(1)/.  Checks if this priority queue is empty.
null :: MinPQueue k a -> Bool
null Empty = True
null _ = False

-- | /O(1)/.  Returns the size of this priority queue.
size :: MinPQueue k a -> Int
size Empty = 0
size (MinPQ n _ _ _) = n

-- | /O(1)/.  Constructs a singleton priority queue.
singleton :: k -> a -> MinPQueue k a
singleton k a = MinPQ 1 k a Nil

-- | Amortized /O(1)/, worst-case /O(log n)/.  Inserts
-- an element with the specified key into the queue.
insert :: Ord k => k -> a -> MinPQueue k a -> MinPQueue k a
insert = insert' (<=)

-- | Internal helper method, using a specific comparator function.
insert' :: LEq k -> k -> a -> MinPQueue k a -> MinPQueue k a
insert' _ k a Empty = singleton k a
insert' (<=) k a (MinPQ n k' a' ts)
	| k <= k'	= MinPQ (n+1) k a (incr (<=) (tip k' a') ts)
	| otherwise	= MinPQ (n+1) k' a' (incr (<=) (tip k a) ts)

-- | Amortized /O(log(min(n1, n2)))/, worst-case /O(log(max(n1, n2)))/.  Returns the union
-- of the two specified queues.
union :: Ord k => MinPQueue k a -> MinPQueue k a -> MinPQueue k a
union = union' (<=)

-- | Takes the union of the two specified queues, using the given comparison function.
union' :: LEq k -> MinPQueue k a -> MinPQueue k a -> MinPQueue k a
union' (<=) (MinPQ n1 k1 a1 ts1) (MinPQ n2 k2 a2 ts2)
	| k1 <= k2	= MinPQ (n1 + n2) k1 a1 (insMerge k2 a2)
	| otherwise	= MinPQ (n1 + n2) k2 a2 (insMerge k1 a1)
	where	insMerge k a = carryForest (<=) (tip k a) ts1 ts2
union' _ Empty q2 = q2
union' _ q1 Empty = q1

-- | /O(1)/.  The minimal (key, element) in the queue, if the queue is nonempty.
getMin :: MinPQueue k a -> Maybe (k, a)
getMin (MinPQ _ k a _) = Just (k, a)
getMin _ = Nothing

-- | /O(1)/.  Alter the value at the minimum key.  If the queue is empty, does nothing.
adjustMinWithKey :: (k -> a -> a) -> MinPQueue k a -> MinPQueue k a
adjustMinWithKey _ Empty = Empty
adjustMinWithKey f (MinPQ n k a ts) = MinPQ n k (f k a) ts

-- | /O(log n)/.  (Actually /O(1)/ if there's no deletion.)  Update the value at the minimum key.
-- If the queue is empty, does nothing.
updateMinWithKey :: Ord k => (k -> a -> Maybe a) -> MinPQueue k a -> MinPQueue k a
updateMinWithKey _ Empty = Empty
updateMinWithKey f (MinPQ n k a ts) = case f k a of
	Nothing	-> extractHeap (<=) n ts
	Just a'	-> MinPQ n k a' ts

-- | /O(log n)/.  Retrieves the minimal (key, value) pair of the map, and the map stripped of that
-- element, or 'Nothing' if passed an empty map.
minViewWithKey :: Ord k => MinPQueue k a -> Maybe ((k, a), MinPQueue k a)
minViewWithKey Empty = Nothing
minViewWithKey (MinPQ n k a ts) = Just ((k, a), extractHeap (<=) n ts)

-- | /O(n)/.  Map a function over all values in the queue.
mapWithKey :: (k -> a -> b) -> MinPQueue k a -> MinPQueue k b
mapWithKey f = runIdentity . traverseWithKeyU (Identity .: f)

-- | /O(n)/.  @'mapKeysMonotonic' f q == 'mapKeys' f q@, but only works when @f@ is strictly
-- monotonic.  /The precondition is not checked./  This function has better performance than
-- 'mapKeys'.
mapKeysMonotonic :: (k -> k') -> MinPQueue k a -> MinPQueue k' a
mapKeysMonotonic _ Empty = Empty
mapKeysMonotonic f (MinPQ n k a ts) = MinPQ n (f k) a (mapKeysMonoF f (const Zero) ts)

-- | /O(n)/.  Map values and collect the 'Just' results.
mapMaybeWithKey :: Ord k => (k -> a -> Maybe b) -> MinPQueue k a -> MinPQueue k b
mapMaybeWithKey _ Empty = Empty
mapMaybeWithKey f (MinPQ _ k a ts) = maybe id (insert k) (f k a) (mapMaybeF (<=) f (const Empty) ts)

-- | /O(n)/.  Map values and separate the 'Left' and 'Right' results.
mapEitherWithKey :: Ord k => (k -> a -> Either b c) -> MinPQueue k a -> (MinPQueue k b, MinPQueue k c)
mapEitherWithKey _ Empty = (Empty, Empty)
mapEitherWithKey f (MinPQ _ k a ts) = either (first' . insert k) (second' . insert k) (f k a) 
	(mapEitherF (<=) f (const (Empty, Empty)) ts)

-- | /O(n log n)/.  Fold the keys and values in the map, such that 
-- @'foldrWithKey' f z q == 'List.foldr' ('uncurry' f) z ('toAscList' q)@.
-- 
-- If you do not care about the traversal order, consider using 'foldrWithKeyU'.
foldrWithKey :: Ord k => (k -> a -> b -> b) -> b -> MinPQueue k a -> b
foldrWithKey _ z Empty = z
foldrWithKey f z (MinPQ _ k a ts) = f k a (foldF ts) where
	extract = extractForest (<=)
	foldF ts = case extract ts of
		Yes (Extract k a _ ts')
			-> f k a (foldF ts')
		_	-> z

-- | /O(n log n)/.  Fold the keys and values in the map, such that 
-- @'foldlWithKey' f z q == 'List.foldl' ('uncurry' . f) z ('toAscList' q)@.
-- 
-- If you do not care about the traversal order, consider using 'foldlWithKeyU'.
foldlWithKey :: Ord k => (b -> k -> a -> b) -> b -> MinPQueue k a -> b
foldlWithKey _ z Empty = z
foldlWithKey f z (MinPQ _ k a ts) = foldF (f z k a) ts where
	extract = extractForest (<=)
	foldF z ts = case extract ts of
		Yes (Extract k a _ ts')
			-> foldF (f z k a) ts'
		_	-> z

-- | Equivalent to 'insert', save the assumption that this key is @<=@
-- every other key in the map.  /The precondition is not checked./
insertMin :: k -> a -> MinPQueue k a -> MinPQueue k a
insertMin k a Empty = MinPQ 1 k a Nil
insertMin k a (MinPQ n k' a' ts) = MinPQ (n+1) k a (incrMin (tip k' a') ts)

-- | /O(1)/.  Returns a binomial tree of rank zero containing this
-- key and value.
tip :: k -> a -> BinomTree Zero k a
tip k a = BinomTree k a Zero

-- | /O(1)/.  Takes the union of two binomial trees of the same rank.
meld :: LEq k -> BinomTree rk k a -> BinomTree rk k a -> BinomTree (Succ rk) k a
meld (<=) t1@(BinomTree k1 v1 ts1) t2@(BinomTree k2 v2 ts2)
	| k1 <= k2	= BinomTree k1 v1 (Succ t2 ts1)
	| otherwise	= BinomTree k2 v2 (Succ t1 ts2)

-- | Takes the union of two binomial forests, starting at the same rank.  Analogous to binary addition.
mergeForest :: LEq k -> BinomForest rk k a -> BinomForest rk k a -> BinomForest rk k a
mergeForest (<=) f1 f2 = case (f1, f2) of
	(Skip ts1, Skip ts2)		-> Skip (mergeForest (<=) ts1 ts2)
	(Skip ts1, Cons t2 ts2)		-> Cons t2 (mergeForest (<=) ts1 ts2)
	(Cons t1 ts1, Skip ts2)		-> Cons t1 (mergeForest (<=) ts1 ts2)
	(Cons t1 ts1, Cons t2 ts2)	-> Skip (carryForest (<=) (meld (<=) t1 t2) ts1 ts2)
	(Nil, _)			-> f2
	(_, Nil)			-> f1

-- | Takes the union of two binomial forests, starting at the same rank, with an additional tree.  
-- Analogous to binary addition when a digit has been carried.
carryForest :: LEq k -> BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a -> BinomForest rk k a
carryForest (<=) t0 f1 f2 = t0 `seq` case (f1, f2) of
	(Cons t1 ts1, Cons t2 ts2)	-> Cons t0 (carryMeld t1 t2 ts1 ts2)
	(Cons t1 ts1, Skip ts2)		-> Skip (carryMeld t0 t1 ts1 ts2)
	(Skip ts1, Cons t2 ts2)		-> Skip (carryMeld t0 t2 ts1 ts2)
	(Skip ts1, Skip ts2)		-> Cons t0 (mergeForest (<=) ts1 ts2)
	(Nil, _)			-> incr (<=) t0 f2
	(_, Nil)			-> incr (<=) t0 f1
	where	carryMeld = carryForest (<=) .: meld (<=)

-- | Inserts a binomial tree into a binomial forest.  Analogous to binary incrementation.
incr :: LEq k -> BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incr (<=) t ts = t `seq` case ts of
	Nil		-> Cons t Nil
	Skip ts'	-> Cons t ts'
	Cons t' ts'	-> Skip (incr (<=) (meld (<=) t t') ts')

-- | Inserts a binomial tree into a binomial forest.  Assumes that the root of this tree
-- is less than all other roots.  Analogous to binary incrementation.  Equivalent to
-- @'incr' (\ _ _ -> True)@.
incrMin :: BinomTree rk k a -> BinomForest rk k a -> BinomForest rk k a
incrMin t@(BinomTree k a ts) tss = case tss of
	Nil		-> Cons t Nil
	Skip tss'	-> Cons t tss'
	Cons t' tss'	-> Skip (incrMin (BinomTree k a (Succ t' ts)) tss')

extractHeap :: LEq k -> Int -> BinomHeap k a -> MinPQueue k a
extractHeap (<=) n ts = n `seq` case extractForest (<=) ts of
	No	-> Empty
	Yes (Extract k a _ ts')
		-> MinPQ (n-1) k a ts'

-- | A specialized type intended to organize the return of extract-min queries
-- from a binomial forest.  We walk all the way through the forest, and then
-- walk backwards.  @Extract rk a@ is the result type of an extract-min 
-- operation that has walked as far backwards of rank @rk@ -- that is, it
-- has visited every root of rank @>= rk@.
-- 
-- The interpretation of @Extract minKey minVal children forest@ is
-- 
-- 	* @minKey@ is the key of the minimum root visited so far.  It may have
-- 		any rank @>= rk@.  We will denote the root corresponding to 
-- 		@minKey@ as @minRoot@.
-- 		
-- 	* @minVal@ is the value corresponding to @minKey@.
-- 	
-- 	* @children@ is those children of @minRoot@ which have not yet been 
-- 		merged with the rest of the forest. Specifically, these are 
-- 		the children with rank @< rk@.
-- 	
-- 	* @forest@ is an accumulating parameter that maintains the partial 
-- 		reconstruction of the binomial forest without @minRoot@. It is 
-- 		the union of all old roots with rank @>= rk@ (except @minRoot@), 
-- 		with the set of all children of @minRoot@ with rank @>= rk@.  
-- 		Note that @forest@ is lazy, so if we discover a smaller key 
-- 		than @minKey@ later, we haven't wasted significant work.

data Extract rk k a = Extract k a (rk k a) (BinomForest rk k a)
data MExtract rk k a = No | Yes {-# UNPACK #-} !(Extract rk k a)

incrExtract :: LEq k -> Maybe (BinomTree rk k a) -> Extract (Succ rk) k a -> Extract rk k a
incrExtract (<=) Nothing (Extract k a (Succ t ts) tss)
	= Extract k a ts (Cons t tss)
incrExtract (<=) (Just t) (Extract k a (Succ t' ts) tss)
	= Extract k a ts (Skip (incr (<=) (meld (<=) t t') tss))

-- | Walks backward from the biggest key in the forest, as far as rank @rk@.
-- Returns its progress.  Each successive application of @extractBin@ takes
-- amortized /O(1)/ time, so applying it from the beginning takes /O(log n)/ time.
extractForest :: LEq k -> BinomForest rk k a -> MExtract rk k a
extractForest _ Nil = No
extractForest (<=) (Skip tss) = case extractForest (<=) tss of
	No	-> No
	Yes ex	-> Yes (incrExtract (<=) Nothing ex)
extractForest (<=) (Cons t@(BinomTree k a ts) tss) = Yes $ case extractForest (<=) tss of
	Yes ex@(Extract k' _ _ _)
		| k' <? k	-> incrExtract (<=) (Just t) ex
	_			-> Extract k a ts (Skip tss)
	where	a <? b = not (b <= a)

-- | Utility function for mapping over a forest.
mapForest :: (k -> a -> b) -> (rk k a -> rk k b) -> BinomForest rk k a -> BinomForest rk k b
mapForest f fCh ts = case ts of
	Nil		-> Nil
	Skip ts'	-> Skip (mapForest f fCh' ts')
	Cons (BinomTree k a ts) tss
		-> Cons (BinomTree k (f k a) (fCh ts)) (mapForest f fCh' tss)
	where	fCh' (Succ (BinomTree k a ts) tss)
			= Succ (BinomTree k (f k a) (fCh ts)) (fCh tss)

-- | Utility function for mapping a 'Maybe' function over a forest.
mapMaybeF :: LEq k -> (k -> a -> Maybe b) -> (rk k a -> MinPQueue k b) ->
	BinomForest rk k a -> MinPQueue k b
mapMaybeF (<=) f fCh ts = case ts of
	Nil		-> Empty
	Skip ts'	-> mapMaybeF (<=) f fCh' ts'
	Cons (BinomTree k a ts) ts'
			-> insF k a (fCh ts) (mapMaybeF (<=) f fCh' ts')
	where	insF k a = maybe id (insert' (<=) k) (f k a) .: union' (<=)
		fCh' (Succ (BinomTree k a ts) tss) =
			insF k a (fCh ts) (fCh tss)

-- | Utility function for mapping an 'Either' function over a forest.
mapEitherF :: LEq k -> (k -> a -> Either b c) -> (rk k a -> (MinPQueue k b, MinPQueue k c)) ->
	BinomForest rk k a -> (MinPQueue k b, MinPQueue k c)
mapEitherF (<=) f fCh ts = case ts of
	Nil		-> (Empty, Empty)
	Skip ts'	-> mapEitherF (<=) f fCh' ts'
	Cons (BinomTree k a ts) ts'
			-> insF k a (fCh ts) (mapEitherF (<=) f fCh' ts')
	where	insF k a = either (first' . insert' (<=) k) (second' . insert' (<=) k) (f k a) .: 
			(union' (<=) `both` union' (<=))
		fCh' (Succ (BinomTree k a ts) tss) =
			insF k a (fCh ts) (fCh tss)
		both f g (x1, x2) (y1, y2) = (f x1 y1, g x2 y2)

-- | /O(n)/.  An unordered right fold over the elements of the queue, in no particular order.
foldrWithKeyU :: (k -> a -> b -> b) -> b -> MinPQueue k a -> b
foldrWithKeyU _ z Empty = z
foldrWithKeyU f z (MinPQ _ k a ts) = f k a (foldrWithKeyF_ f (const id) ts z)

-- | /O(n)/.  An unordered left fold over the elements of the queue, in no particular order.
foldlWithKeyU :: (b -> k -> a -> b) -> b -> MinPQueue k a -> b
foldlWithKeyU _ z Empty = z
foldlWithKeyU f z (MinPQ _ k a ts) = foldlWithKeyF_ (\ k a z -> f z k a) (const id) ts (f z k a)

traverseWithKeyU :: Applicative f => (k -> a -> f b) -> MinPQueue k a -> f (MinPQueue k b)
traverseWithKeyU _ Empty = pure Empty
traverseWithKeyU f (MinPQ n k a ts) = MinPQ n k <$> f k a <*> traverseForest f (const (pure Zero)) ts

{-# SPECIALIZE traverseForest :: (k -> a -> Identity b) -> (rk k a -> Identity (rk k b)) -> BinomForest rk k a ->
	Identity (BinomForest rk k b) #-}
traverseForest :: (Applicative f) => (k -> a -> f b) -> (rk k a -> f (rk k b)) -> BinomForest rk k a -> f (BinomForest rk k b)
traverseForest f fCh ts = case ts of
	Nil		-> pure Nil
	Skip ts'	-> Skip <$> traverseForest f fCh' ts'
	Cons (BinomTree k a ts) tss
		-> Cons <$> (BinomTree k <$> f k a <*> fCh ts) <*> traverseForest f fCh' tss
	where	fCh' (Succ (BinomTree k a ts) tss)
			= Succ <$> (BinomTree k <$> f k a <*> fCh ts) <*> fCh tss

-- | Unordered right fold on a binomial forest.
foldrWithKeyF_ :: (k -> a -> b -> b) -> (rk k a -> b -> b) -> BinomForest rk k a -> b -> b
foldrWithKeyF_ f fCh ts z = case ts of
	Nil		-> z
	Skip ts'	-> foldrWithKeyF_ f fCh' ts' z
	Cons (BinomTree k a ts) ts'
		-> f k a (fCh ts (foldrWithKeyF_ f fCh' ts' z))
	where	fCh' (Succ (BinomTree k a ts) tss) z =
			f k a (fCh ts (fCh tss z))

-- | Unordered left fold on a binomial forest.
foldlWithKeyF_ :: (k -> a -> b -> b) -> (rk k a -> b -> b) -> BinomForest rk k a -> b -> b
foldlWithKeyF_ f fCh ts = case ts of
	Nil		-> id
	Skip ts'	-> foldlWithKeyF_ f fCh' ts'
	Cons (BinomTree k a ts) ts'
		-> foldlWithKeyF_ f fCh' ts' . fCh ts . f k a
	where	fCh' (Succ (BinomTree k a ts) tss) =
			fCh tss . fCh ts . f k a

-- | Maps a monotonic function over the keys in a binomial forest.
mapKeysMonoF :: (k -> k') -> (rk k a -> rk k' a) -> BinomForest rk k a -> BinomForest rk k' a
mapKeysMonoF f fCh ts = case ts of
	Nil		-> Nil
	Skip ts'	-> Skip (mapKeysMonoF f fCh' ts')
	Cons (BinomTree k a ts) ts'
		-> Cons (BinomTree (f k) a (fCh ts)) (mapKeysMonoF f fCh' ts')
	where	fCh' (Succ (BinomTree k a ts) tss) =
			Succ (BinomTree (f k) a (fCh ts)) (fCh tss)

-- | /O(log n)/.  Analogous to @deepseq@ in the @deepseq@ package, but only forces the spine of the binomial heap.
seqSpine :: MinPQueue k a -> b -> b
seqSpine Empty z = z
seqSpine (MinPQ _ _ _ ts) z = ts `seqSpineF` z where
	seqSpineF :: BinomForest rk k a -> b -> b
	seqSpineF ts z = case ts of
		Nil		-> z
		Skip ts'	-> seqSpineF ts' z
		Cons _ ts'	-> seqSpineF ts' z

-- class NFRank rk where
-- 	rnfRk :: (NFData k, NFData a) => rk k a -> ()

-- instance NFRank Zero where
-- 	rnfRk _ = ()

-- instance NFRank rk => NFRank (Succ rk) where
-- 	rnfRk (Succ t ts) = t `deepseq` rnfRk ts

-- instance (NFData k, NFData a, NFRank rk) => NFData (BinomTree rk k a) where
-- 	rnf (BinomTree k a ts) = k `deepseq` a `deepseq` rnfRk ts

-- instance (NFData k, NFData a, NFRank rk) => NFData (BinomForest rk k a) where
-- 	rnf Nil = ()
-- 	rnf (Skip tss) = rnf tss
-- 	rnf (Cons t tss) = t `deepseq` rnf tss

-- instance (NFData k, NFData a) => NFData (MinPQueue k a) where
-- 	rnf Empty = ()
-- 	rnf (MinPQ _ k a ts) = k `deepseq` a `deepseq` rnf ts
