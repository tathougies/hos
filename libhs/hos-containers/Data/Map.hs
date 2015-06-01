{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Map
-- Copyright   :  (c) Daan Leijen 2002
--                (c) Andriy Palamarchuk 2008
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- /Note:/ You should use "Data.Map.Strict" instead of this module if:
--
-- * You will eventually need all the values stored.
--
-- * The stored values don't represent large virtual data structures
-- to be lazily computed.
--
-- An efficient implementation of ordered maps from keys to values
-- (dictionaries).
--
-- These modules are intended to be imported qualified, to avoid name
-- clashes with Prelude functions, e.g.
--
-- >  import qualified Data.Map as Map
--
-- The implementation of 'Map' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--     Journal of Functional Programming 3(4):553-562, October 1993,
--     <http://www.swiss.ai.mit.edu/~adams/BB/>.
--
--    * J. Nievergelt and E.M. Reingold,
--      \"/Binary search trees of bounded balance/\",
--      SIAM journal of computing 2(1), March 1973.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.
--
-- /Warning/: The size of the map must not exceed @maxBound::Int@. Violation of
-- this condition is not detected and if the size limit is exceeded, its
-- behaviour is undefined.
--
-- Operation comments contain the operation time complexity in
-- the Big-O notation (<http://en.wikipedia.org/wiki/Big_O_notation>).
-----------------------------------------------------------------------------

module Data.Map
    ( module Data.Map.Lazy
    ) where

import Prelude hiding (foldr)
import Data.Map.Lazy
-- import qualified Data.Map.Strict as Strict

-- | /Deprecated./ As of version 0.5, replaced by 'Data.Map.Strict.insertWith'.
--
-- /O(log n)/. Same as 'insertWith', but the value being inserted to the map is
-- evaluated to WHNF beforehand.
--
-- For example, to update a counter:
--
-- > insertWith' (+) k 1 m
--
