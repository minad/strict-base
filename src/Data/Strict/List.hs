{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.List
-- Copyright   :  (c) 2017 Daniel Mendler
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Daniel Mendler <mail@daniel-mendler.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict @List@.
--
-- Same as the standard Haskell List, but strict.
--
-----------------------------------------------------------------------------

module Data.Strict.List (
  List(..)
) where

import GHC.Generics (Generic, Generic1)
import Data.Data (Data, Typeable)
import Data.Strict.Trustworthy
import Data.Strict.Class
import Data.Semigroup (Semigroup, (<>))

infixr 5 :!

-- | The strict list type.
data List a = Nil | !a :! !(List a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance IsStrict (List a) where
  type Lazy (List a) = [a]
  toStrict   = foldr (:!) Nil
  fromStrict = foldr (:) []

instance IsList (List a) where
  type Item (List a) = a
  fromList = toStrict
  toList   = fromStrict

instance Applicative List where
  {-# INLINE pure #-}
  pure x = x :! Nil
  {-# INLINE (<*>) #-}
  fl <*> xl = go fl where
    go (f:!fs) = go' xl where
      go' (x:!xs) = (f x) :! go' xs
      go' Nil = go fs
    go Nil = Nil

#if MIN_VERSION_base(4,10,0)
  {-# INLINE liftA2 #-}
  liftA2 f xl yl = go xl where
    go (x:!xs) = go' yl where
      go' (y:!ys) = (f x y) :! go' ys
      go' Nil = go xs
    go Nil = Nil
#endif

instance Semigroup (List a) where
  (<>) xl yl = go xl where
    go (x:!xs) = x :! go xs
    go Nil = yl

instance Monoid (List a) where
  mappend = (<>)
  mempty = Nil
