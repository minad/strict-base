{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Tuple
-- Copyright   :  (c) 2017 Daniel Mendler, 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Daniel Mendler <mail@daniel-mendler.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict pairs.
--
-- Same as regular Haskell pairs, but @(x :!: _|_) = (_|_ :!: y) = _|_@
--
-----------------------------------------------------------------------------

module Data.Strict.Tuple (
    Pair(..)
  , (:!:)
  , fst
  , snd
  , curry
  , uncurry
  , swap
  , zip
  , unzip
) where

import Prelude hiding (fst, snd, curry, uncurry, zip, unzip)
import Data.Semigroup(Semigroup(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Ix (Ix)
import GHC.Generics (Generic, Generic1)
import Data.Data (Data, Typeable)
import Data.Strict.Class

infixl 2 :!:

-- | The type of strict pairs.
data Pair a b = !a :!: !b
  deriving (Eq, Ord, Show, Read, Bounded, Ix, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

-- This gives a nicer syntax for the type but only works in GHC for now.
type (:!:) = Pair

instance IsStrict (Pair a b) where
  type Lazy (Pair a b) = (a,b)
  toStrict   (a, b)    = a :!: b
  fromStrict (a :!: b) = (a, b)

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  (x1 :!: y1) <> (x2 :!: y2) = (x1 <> x2) :!: (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty                            = mempty :!: mempty
  (x1 :!: y1) `mappend` (x2 :!: y2) = (x1 `mappend` x2) :!: (y1 `mappend` y2)
instance Bifunctor Pair where
  bimap f g (a :!: b) = f a :!: g b
  first f (a :!: b) = f a :!: b
  second g (a :!: b) = a :!: g b

-- | Extract the first component of a strict pair.
fst :: Pair a b -> a
fst (x :!: _) = x

-- | Extract the second component of a strict pair.
snd :: Pair a b -> b
snd (_ :!: y) = y

-- | Curry a function on strict pairs.
curry :: (Pair a b -> c) -> a -> b -> c
curry f x y = f (x :!: y)

-- | Convert a curried function to a function on strict pairs.
uncurry :: (a -> b -> c) -> Pair a b -> c
uncurry f (x :!: y) = f x y

-- | Analagous to 'L.swap' from "Data.Tuple"
swap :: Pair a b -> Pair b a
swap (a :!: b) = b :!: a

-- | Zip for strict pairs (defined with zipWith).
zip :: [a] -> [b] -> [Pair a b]
zip = zipWith (:!:)

-- | Unzip for stict pairs into a (lazy) pair of lists.
unzip :: [Pair a b] -> ([a], [b])
unzip x = (map fst x, map snd x)
