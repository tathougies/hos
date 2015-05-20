{-# LANGUAGE CPP, StandaloneDeriving #-}

module Data.PQueue.Internals (
	MinQueue (..),
	BinomHeap,
	BinomForest(..),
	BinomTree(..),
	Succ(..),
	Zero(..),
	LEq,
	empty,
	null,
	size,
	getMin,
	minView,
	singleton,
	insert,
	union,
	mapMaybe,
	mapEither,
	mapMonotonic,
	foldrAsc,
	foldlAsc,
	insertMinQ,
-- 	mapU,
	foldrU,
	foldlU,
-- 	traverseU,
	keysQueue,
	seqSpine
	) where


import Data.Functor
import Data.Foldable (Foldable (..))
import Data.Monoid (Monoid (..))
import qualified Data.PQueue.Prio.Internals as Prio

#ifdef __GLASGOW_HASKELL__
import Data.Data
#endif

import Prelude hiding (foldl, foldr, null)

-- | A priority queue with elements of type @a@.  Supports extracting the minimum element.
data MinQueue a = Empty | MinQueue {-# UNPACK #-} !Int a !(BinomHeap a)

#ifdef __GLASGOW_HASKELL__
instance (Ord a, Data a) => Data (MinQueue a) where
	gfoldl f z q	= case minView q of
		Nothing	-> z Empty
		Just (x, q')
			-> z insertMinQ `f` x `f` q'
	
	gunfold k z c = case constrIndex c of
		1	-> z Empty
		2	-> k (k (z insertMinQ))
		_	-> error "gunfold"
	
	dataCast1 x = gcast1 x
	
	toConstr q
		| null q	= emptyConstr
		| otherwise	= consConstr

	dataTypeOf _ = queueDataType

queueDataType :: DataType
queueDataType = mkDataType "Data.PQueue.Min.MinQueue" [emptyConstr, consConstr]

emptyConstr, consConstr :: Constr
emptyConstr = mkConstr queueDataType "empty" [] Prefix
consConstr  = mkConstr queueDataType "<|" [] Infix

#include "Typeable.h"
INSTANCE_TYPEABLE1(MinQueue,minQTC,"MinQueue")
#endif

type BinomHeap = BinomForest Zero

instance Ord a => Eq (MinQueue a) where
	Empty == Empty = True
	MinQueue n1 x1 q1 == MinQueue n2 x2 q2 = n1 == n2 && x1 == x2 && eq' q1 q2 where
		eq' q1 q2 = case (extractHeap q1, extractHeap q2) of
			(Just (x1, q1'), Just (x2, q2'))
				-> x1 == x2 && eq' q1' q2'
			(Nothing, Nothing)
				-> True
			_	-> False
	_ == _ = False

instance Ord a => Ord (MinQueue a) where
	Empty `compare` Empty = EQ
	Empty `compare` _ = LT
	_ `compare` Empty = GT
	MinQueue n1 x1 q1 `compare` MinQueue n2 x2 q2 = compare x1 x2 `mappend` cmp' q1 q2 where
		cmp' q1 q2 = case (extractHeap q1, extractHeap q2) of
			(Just (x1, q1'), Just (x2, q2'))
				-> compare x1 x2 `mappend` cmp' q1' q2'
			(Nothing, Nothing)
				-> EQ
			(Just{}, Nothing)
				-> GT
			(Nothing, Just{})
				-> LT
			
		-- We compare their first elements, then their other elements up to the smaller queue's length,
		-- and then the longer queue wins.
		-- This is equivalent to @comparing toAscList@, except it fuses much more nicely.

-- We implement tree ranks in the type system with a nicely elegant approach, as follows.
-- The goal is to have the type system automatically guarantee that our binomial forest
-- has the correct binomial structure.
-- 
-- In the traditional set-theoretic construction of the natural numbers, we define
-- each number to be the set of numbers less than it, and Zero to be the empty set,
-- as follows:
-- 
-- 0 = {}	1 = {0}		2 = {0, 1}	3={0, 1, 2} ...
-- 
-- Binomial trees have a similar structure: a tree of rank @k@ has one child of each
-- rank less than @k@.  Let's define the type @rk@ corresponding to rank @k@ to refer
-- to a collection of binomial trees of ranks @0..k-1@.  Then we can say that
-- 
-- > data Succ rk a = Succ (BinomTree rk a) (rk a)
-- 
-- and this behaves exactly as the successor operator for ranks should behave.  Furthermore,
-- we immediately obtain that
-- 
-- > data BinomTree rk a = BinomTree a (rk a)
-- 
-- which is nice and compact.  With this construction, things work out extremely nicely:
-- 
-- > BinomTree (Succ (Succ (Succ Zero)))
-- 
-- is a type constructor that takes an element type and returns the type of binomial trees
-- of rank @3@.
data BinomForest rk a = Nil | Skip (BinomForest (Succ rk) a) | 
	Cons {-# UNPACK #-} !(BinomTree rk a) (BinomForest (Succ rk) a)

data BinomTree rk a = BinomTree a (rk a)

-- | If |rk| corresponds to rank @k@, then |'Succ' rk| corresponds to rank @k+1@.
data Succ rk a = Succ {-# UNPACK #-} !(BinomTree rk a) (rk a)

-- | Type corresponding to the Zero rank.
data Zero a = Zero

-- | Type alias for a comparison function.
type LEq a = a -> a -> Bool

-- basics

-- | /O(1)/.  The empty priority queue.
empty :: MinQueue a
empty = Empty

-- | /O(1)/.  Is this the empty priority queue?
null :: MinQueue a -> Bool
null Empty = True
null _ = False

-- | /O(1)/.  The number of elements in the queue.
size :: MinQueue a -> Int
size Empty = 0
size (MinQueue n _ _) = n

-- | Returns the minimum element of the queue, if the queue is nonempty.
getMin :: MinQueue a -> Maybe a
getMin (MinQueue _ x _) = Just x
getMin _ = Nothing

-- | Retrieves the minimum element of the queue, and the queue stripped of that element, 
-- or 'Nothing' if passed an empty queue.
minView :: Ord a => MinQueue a -> Maybe (a, MinQueue a)
minView Empty = Nothing
minView (MinQueue n x ts) = Just (x, case extractHeap ts of
	Nothing		-> Empty
	Just (x', ts')	-> MinQueue (n-1) x' ts')

-- | /O(1)/.  Construct a priority queue with a single element.
singleton :: a -> MinQueue a
singleton x = MinQueue 1 x Nil

-- | Amortized /O(1)/, worst-case /O(log n)/.  Insert an element into the priority queue.  
insert :: Ord a => a -> MinQueue a -> MinQueue a
insert = insert' (<=)

-- | Amortized /O(log (min(n,m)))/, worst-case /O(log (max (n,m)))/.  Take the union of two priority queues.
union :: Ord a => MinQueue a -> MinQueue a -> MinQueue a
union = union' (<=)

-- | /O(n)/.  Map elements and collect the 'Just' results.
mapMaybe :: Ord b => (a -> Maybe b) -> MinQueue a -> MinQueue b
mapMaybe _ Empty = Empty
mapMaybe f (MinQueue _ x ts) = maybe q' (`insert` q') (f x)
	where	q' = mapMaybeQueue f (<=) (const Empty) Empty ts

-- | /O(n)/.  Map elements and separate the 'Left' and 'Right' results.
mapEither :: (Ord b, Ord c) => (a -> Either b c) -> MinQueue a -> (MinQueue b, MinQueue c)
mapEither _ Empty = (Empty, Empty)
mapEither f (MinQueue _ x ts) = case (mapEitherQueue f (<=) (<=) (const (Empty, Empty)) (Empty, Empty) ts, f x) of
	((qL, qR), Left b)	-> (insert b qL, qR)
	((qL, qR), Right c)	-> (qL, insert c qR)

-- | /O(n)/.  Assumes that the function it is given is monotonic, and applies this function to every element of the priority queue,
-- as in 'fmap'.  If it is not, the result is undefined.
mapMonotonic :: (a -> b) -> MinQueue a -> MinQueue b
mapMonotonic = mapU

{-# INLINE foldrAsc #-}
-- | /O(n log n)/.  Performs a right-fold on the elements of a priority queue in ascending order.
foldrAsc :: Ord a => (a -> b -> b) -> b -> MinQueue a -> b
foldrAsc _ z Empty = z
foldrAsc f z (MinQueue _ x ts) = x `f` foldrUnfold f z extractHeap ts

{-# INLINE foldrUnfold #-}
-- | Equivalent to @foldr f z (unfoldr suc s0)@.
foldrUnfold :: (a -> c -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
foldrUnfold f z suc s0 = unf s0 where
	unf s = case suc s of
		Nothing		-> z
		Just (x, s')	-> x `f` unf s'

-- | /O(n log n)/.  Performs a left-fold on the elements of a priority queue in ascending order.
foldlAsc :: Ord a => (b -> a -> b) -> b -> MinQueue a -> b
foldlAsc _ z Empty = z
foldlAsc f z (MinQueue _ x ts) = foldlUnfold f (z `f` x) extractHeap ts

{-# INLINE foldlUnfold #-}
-- | @foldlUnfold f z suc s0@ is equivalent to @foldl f z (unfoldr suc s0)@.
foldlUnfold :: (c -> a -> c) -> c -> (b -> Maybe (a, b)) -> b -> c
foldlUnfold f z suc s0 = unf z s0 where
	unf z s = case suc s of
		Nothing		-> z
		Just (x, s')	-> unf (z `f` x) s'
insert' :: LEq a -> a -> MinQueue a -> MinQueue a
insert' _ x Empty = singleton x
insert' (<=) x (MinQueue n x' ts)
	| x <= x'	= MinQueue (n+1) x (incr (<=) (tip x') ts)
	| otherwise	= MinQueue (n+1) x' (incr (<=) (tip x) ts)

{-# INLINE union' #-}
union' :: LEq a -> MinQueue a -> MinQueue a -> MinQueue a
union' _ Empty q = q
union' _ q Empty = q
union' (<=) (MinQueue n1 x1 f1) (MinQueue n2 x2 f2)
	| x1 <= x2	= MinQueue (n1 + n2) x1 (carry (<=) (tip x2) f1 f2)
	| otherwise	= MinQueue (n1 + n2) x2 (carry (<=) (tip x1) f1 f2)

-- | Takes a size and a binomial forest and produces a priority queue with a distinguished global root.
extractHeap :: Ord a => BinomHeap a -> Maybe (a, BinomHeap a)
extractHeap ts = case extractBin (<=) ts of
	Yes (Extract x _ ts')	-> Just (x, ts')
	_			-> Nothing

-- | A specialized type intended to organize the return of extract-min queries
-- from a binomial forest.  We walk all the way through the forest, and then
-- walk backwards.  @Extract rk a@ is the result type of an extract-min 
-- operation that has walked as far backwards of rank @rk@ -- that is, it
-- has visited every root of rank @>= rk@.
-- 
-- The interpretation of @Extract minKey children forest@ is
-- 
-- 	* @minKey@ is the key of the minimum root visited so far.  It may have
-- 		any rank @>= rk@.  We will denote the root corresponding to 
-- 		@minKey@ as @minRoot@.
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
data Extract rk a = Extract a (rk a) (BinomForest rk a)
data MExtract rk a = No | Yes {-# UNPACK #-} !(Extract rk a)

incrExtract :: Extract (Succ rk) a -> Extract rk a
incrExtract (Extract minKey (Succ kChild kChildren) ts)
	= Extract minKey kChildren (Cons kChild ts)

incrExtract' :: LEq a -> BinomTree rk a -> Extract (Succ rk) a -> Extract rk a
incrExtract' (<=) t (Extract minKey (Succ kChild kChildren) ts)
	= Extract minKey kChildren (Skip (incr (<=) (t `cat` kChild) ts))
	where	cat = joinBin (<=)

-- | Walks backward from the biggest key in the forest, as far as rank @rk@.
-- Returns its progress.  Each successive application of @extractBin@ takes
-- amortized /O(1)/ time, so applying it from the beginning takes /O(log n)/ time.
extractBin :: LEq a -> BinomForest rk a -> MExtract rk a
extractBin _ Nil = No
extractBin (<=) (Skip f) = case extractBin (<=) f of
	Yes ex	-> Yes (incrExtract ex)
	No	-> No
extractBin (<=) (Cons t@(BinomTree x ts) f) = Yes $ case extractBin (<=) f of
	Yes ex@(Extract minKey _ _)
		| minKey < x	-> incrExtract' (<=) t ex
	_			-> Extract x ts (Skip f)
	where	a < b = not (b <= a)

mapMaybeQueue :: (a -> Maybe b) -> LEq b -> (rk a -> MinQueue b) -> MinQueue b -> BinomForest rk a -> MinQueue b
mapMaybeQueue f (<=) fCh q0 forest = q0 `seq` case forest of
	Nil		-> q0
	Skip forest'	-> mapMaybeQueue f (<=) fCh' q0 forest'
	Cons t forest'	-> mapMaybeQueue f (<=) fCh' (union' (<=) (mapMaybeT t) q0) forest'
	where	fCh' (Succ t tss) = union' (<=) (mapMaybeT t) (fCh tss)
		mapMaybeT (BinomTree x ts) = maybe (fCh ts) (\ x -> insert' (<=) x (fCh ts)) (f x)

type Partition a b = (MinQueue a, MinQueue b)

mapEitherQueue :: (a -> Either b c) -> LEq b -> LEq c -> (rk a -> Partition b c) -> Partition b c ->
	BinomForest rk a -> Partition b c
mapEitherQueue f (<=) (<=.) fCh (q0, q1) ts = q0 `seq` q1 `seq` case ts of
	Nil		-> (q0, q1)
	Skip ts'	-> mapEitherQueue f (<=) (<=.) fCh' (q0, q1) ts'
	Cons t ts'	-> mapEitherQueue f (<=) (<=.) fCh' (both (union' (<=)) (union' (<=.)) (partitionT t) (q0, q1)) ts'
	where	both f g (x1, x2) (y1, y2) = (f x1 y1, g x2 y2)
		fCh' (Succ t tss) = both (union' (<=)) (union' (<=.)) (partitionT t) (fCh tss)
		partitionT (BinomTree x ts) = case fCh ts of
			(q0, q1) -> case f x of
				Left b	-> (insert' (<=) b q0, q1)
				Right c	-> (q0, insert' (<=.) c q1)

{-# INLINE tip #-}
-- | Constructs a binomial tree of rank 0.
tip :: a -> BinomTree Zero a
tip x = BinomTree x Zero

insertMinQ :: a -> MinQueue a -> MinQueue a
insertMinQ x Empty = singleton x
insertMinQ x (MinQueue n x' f) = MinQueue (n+1) x (insertMin (tip x') f)

-- | @insertMin t f@ assumes that the root of @t@ compares as less than
-- every other root in @f@, and merges accordingly.
insertMin :: BinomTree rk a -> BinomForest rk a -> BinomForest rk a
insertMin t Nil = Cons t Nil
insertMin t (Skip f) = Cons t f
insertMin (BinomTree x ts) (Cons t' f) = Skip (insertMin (BinomTree x (Succ t' ts)) f)

-- | Given two binomial forests starting at rank @rk@, takes their union.
-- Each successive application of this function costs /O(1)/, so applying it
-- from the beginning costs /O(log n)/.
merge :: LEq a -> BinomForest rk a -> BinomForest rk a -> BinomForest rk a
merge (<=) f1 f2 = case (f1, f2) of
	(Skip f1', Skip f2')	-> Skip (merge (<=) f1' f2')
	(Skip f1', Cons t2 f2')	-> Cons t2 (merge (<=) f1' f2')
	(Cons t1 f1', Skip f2')	-> Cons t1 (merge (<=) f1' f2')
	(Cons t1 f1', Cons t2 f2')
				-> Skip (carry (<=) (t1 `cat` t2) f1' f2')
	(Nil, _)		-> f2
	(_, Nil)		-> f1
	where	cat = joinBin (<=)

-- | Merges two binomial forests with another tree. If we are thinking of the trees 
-- in the binomial forest as binary digits, this corresponds to a carry operation.
-- Each call to this function takes /O(1)/ time, so in total, it costs /O(log n)/.
carry :: LEq a -> BinomTree rk a -> BinomForest rk a -> BinomForest rk a -> BinomForest rk a
carry (<=) t0 f1 f2 = t0 `seq` case (f1, f2) of
	(Skip f1', Skip f2')	-> Cons t0 (merge (<=) f1' f2')
	(Skip f1', Cons t2 f2')	-> Skip (mergeCarry t0 t2 f1' f2')
	(Cons t1 f1', Skip f2')	-> Skip (mergeCarry t0 t1 f1' f2')
	(Cons t1 f1', Cons t2 f2')
				-> Cons t0 (mergeCarry t1 t2 f1' f2')
	(Nil, _f2)		-> incr (<=) t0 f2
	(_f1, Nil)		-> incr (<=) t0 f1
	where	cat = joinBin (<=)
		mergeCarry tA tB = carry (<=) (tA `cat` tB)

-- | Merges a binomial tree into a binomial forest.  If we are thinking
-- of the trees in the binomial forest as binary digits, this corresponds
-- to adding a power of 2.  This costs amortized /O(1)/ time.
incr :: LEq a -> BinomTree rk a -> BinomForest rk a -> BinomForest rk a
incr (<=) t f = t `seq` case f of
	Nil	-> Cons t Nil
	Skip f	-> Cons t f
	Cons t' f' -> Skip (incr (<=) (t `cat` t') f')
	where	cat = joinBin (<=)

-- | The carrying operation: takes two binomial heaps of the same rank @k@
-- and returns one of rank @k+1@.  Takes /O(1)/ time.
joinBin :: LEq a -> BinomTree rk a -> BinomTree rk a -> BinomTree (Succ rk) a
joinBin (<=) t1@(BinomTree x1 ts1) t2@(BinomTree x2 ts2)
	| x1 <= x2	= BinomTree x1 (Succ t2 ts1)
	| otherwise	= BinomTree x2 (Succ t1 ts2)

instance Functor Zero where
	fmap _ _ = Zero

instance Functor rk => Functor (Succ rk) where
	fmap f (Succ t ts) = Succ (fmap f t) (fmap f ts)

instance Functor rk => Functor (BinomTree rk) where
	fmap f (BinomTree x ts) = BinomTree (f x) (fmap f ts)

instance Functor rk => Functor (BinomForest rk) where
	fmap _ Nil = Nil
	fmap f (Skip ts) = Skip (fmap f ts)
	fmap f (Cons t ts) = Cons (fmap f t) (fmap f ts)

instance Foldable Zero where
	foldr _ z _ = z
	foldl _ z _ = z

instance Foldable rk => Foldable (Succ rk) where
	foldr f z (Succ t ts) = foldr f (foldr f z ts) t
	foldl f z (Succ t ts) = foldl f (foldl f z t) ts

instance Foldable rk => Foldable (BinomTree rk) where
	foldr f z (BinomTree x ts) = x `f` foldr f z ts
	foldl f z (BinomTree x ts) = foldl f (z `f` x) ts

instance Foldable rk => Foldable (BinomForest rk) where
	foldr _ z Nil = z
	foldr f z (Skip tss) = foldr f z tss
	foldr f z (Cons t tss) = foldr f (foldr f z tss) t
	foldl _ z Nil = z
	foldl f z (Skip tss) = foldl f z tss
	foldl f z (Cons t tss) = foldl f (foldl f z t) tss

-- instance Traversable Zero where
-- 	traverse _ _ = pure Zero
-- 
-- instance Traversable rk => Traversable (Succ rk) where
-- 	traverse f (Succ t ts) = Succ <$> traverse f t <*> traverse f ts
-- 
-- instance Traversable rk => Traversable (BinomTree rk) where
-- 	traverse f (BinomTree x ts) = BinomTree <$> f x <*> traverse f ts
-- 
-- instance Traversable rk => Traversable (BinomForest rk) where
-- 	traverse _ Nil = pure Nil
-- 	traverse f (Skip tss) = Skip <$> traverse f tss
-- 	traverse f (Cons t tss) = Cons <$> traverse f t <*> traverse f tss

mapU :: (a -> b) -> MinQueue a -> MinQueue b
mapU _ Empty = Empty
mapU f (MinQueue n x ts) = MinQueue n (f x) (f <$> ts)

-- | /O(n)/.  Unordered right fold on a priority queue.
foldrU :: (a -> b -> b) -> b -> MinQueue a -> b
foldrU _ z Empty = z
foldrU f z (MinQueue _ x ts) = x `f` foldr f z ts

-- | /O(n)/.  Unordered left fold on a priority queue.
foldlU :: (b -> a -> b) -> b -> MinQueue a -> b
foldlU _ z Empty = z
foldlU f z (MinQueue _ x ts) = foldl f (z `f` x) ts

-- traverseU :: Applicative f => (a -> f b) -> MinQueue a -> f (MinQueue b)
-- traverseU _ Empty = pure Empty
-- traverseU f (MinQueue n x ts) = MinQueue n <$> f x <*> traverse f ts

-- | Forces the spine of the priority queue.
seqSpine :: MinQueue a -> b -> b
seqSpine Empty z = z
seqSpine (MinQueue _ _ ts) z = seqSpineF ts z

seqSpineF :: BinomForest rk a -> b -> b
seqSpineF Nil z = z
seqSpineF (Skip ts') z = seqSpineF ts' z
seqSpineF (Cons _ ts') z = seqSpineF ts' z

-- | Constructs a priority queue out of the keys of the specified 'Prio.MinPQueue'.
keysQueue :: Prio.MinPQueue k a -> MinQueue k
keysQueue Prio.Empty = Empty
keysQueue (Prio.MinPQ n k _ ts) = MinQueue n k (keysF (const Zero) ts)

keysF :: (pRk k a -> rk k) -> Prio.BinomForest pRk k a -> BinomForest rk k
keysF f ts = case ts of
	Prio.Nil	-> Nil
	Prio.Skip ts'	-> Skip (keysF f' ts')
	Prio.Cons (Prio.BinomTree k _ ts) ts'
		-> Cons (BinomTree k (f ts)) (keysF f' ts')
	where	f' (Prio.Succ (Prio.BinomTree k _ ts) tss) = Succ (BinomTree k (f ts)) (f tss)

-- class NFRank rk where
-- 	rnfRk :: NFData a => rk a -> ()

-- instance NFRank Zero where
-- 	rnfRk _ = ()

-- instance NFRank rk => NFRank (Succ rk) where
-- 	rnfRk (Succ t ts) = t `deepseq` rnfRk ts

-- instance (NFData a, NFRank rk) => NFData (BinomTree rk a) where
-- 	rnf (BinomTree x ts) = x `deepseq` rnfRk ts

-- instance (NFData a, NFRank rk) => NFData (BinomForest rk a) where
-- 	rnf Nil = ()
-- 	rnf (Skip ts) = rnf ts
-- 	rnf (Cons t ts) = t `deepseq` rnf ts

-- instance NFData a => NFData (MinQueue a) where
-- 	rnf Empty = ()
-- 	rnf (MinQueue _ x ts) = x `deepseq` rnf ts
