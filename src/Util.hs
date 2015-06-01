module Util where

second :: (a -> b) -> (c, a) -> (c, b)
second f (x, y) = (x, f y)
