{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Either
-- Copyright   :  (c) 2017 Daniel Mendler, 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Daniel Mendler <mail@daniel-mendler.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict @Either@.
--
-- Same as the standard Haskell @Either@, but @Left _|_ = Right _|_ = _|_@
--
-----------------------------------------------------------------------------

module Data.Strict.Either (
    Either(..)
  , either
  , isLeft, isRight
  , lefts, rights
  , partitionEithers
) where

import qualified Data.Either as L
import Prelude hiding (Either(..), either)
import Data.Bifunctor (Bifunctor(..))
import GHC.Generics (Generic, Generic1)
import Data.Data (Data, Typeable)
import Data.Strict.Class

-- | The strict choice type.
data Either a b = Left !a | Right !b
  deriving (Eq, Ord, Read, Show, Functor, Traversable, Foldable, Generic, Generic1, Data, Typeable)

instance IsStrict (Either a b) where
  type Lazy (Either a b) = (L.Either a b)
  toStrict   (L.Left x)  = Left x
  toStrict   (L.Right y) = Right y
  fromStrict (Left x)    = L.Left x
  fromStrict (Right y)   = L.Right y

instance Bifunctor Either where
  bimap f _ (Left a) = Left (f a)
  bimap _ g (Right a) = Right (g a)
  first f = either (Left . f) Right
  second g = either Left (Right . g)

-- | Case analysis: if the value is @'Left' a@, apply the first function to @a@;
-- if it is @'Right' b@, apply the second function to @b@.
either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left  x) = f x
either _ g (Right y) = g y

-- | Yields 'True' iff the argument is of the form @Left _@.
--
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

-- | Yields 'True' iff the argument is of the form @Right _@.
--
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

-- | Analogous to 'L.lefts' in "Data.Either".
lefts :: [Either a b] -> [a]
lefts x = [a | Left a <- x]

-- | Analogous to 'L.rights' in "Data.Either".
rights :: [Either a b] -> [b]
rights x = [a | Right a <- x]

-- | Analogous to 'L.partitionEithers' in "Data.Either".
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers = foldr (either left right) ([], [])
  where left  a ~(l, r) = (a:l, r)
        right a ~(l, r) = (l, a:r)
