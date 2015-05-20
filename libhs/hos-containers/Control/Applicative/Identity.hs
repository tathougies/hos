module Control.Applicative.Identity where

import Control.Applicative

newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
	fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
	pure = Identity
	Identity f <*> Identity x = Identity (f x)