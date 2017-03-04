{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.List
-- Copyright   :  (c) 2017 Daniel Mendler
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
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
  , toStrictList
  , toLazyList
) where

import GHC.Generics (Generic, Generic1)
import Data.Data (Data, Typeable)
import Data.Strict.Trustworthy

infixr 5 :!

-- | The strict list type.
data List a = Nil | !a :! !(List a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance IsList (List a) where
  type Item (List a) = a
  fromList = toStrictList
  toList   = toLazyList

toStrictList :: [a] -> List a
toStrictList = foldr (:!) Nil

toLazyList :: List a -> [a]
toLazyList = foldr (:) []
