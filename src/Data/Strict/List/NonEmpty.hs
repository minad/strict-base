{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.List.NonEmpty
-- Copyright   :  (c) 2017 Daniel Mendler
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict @NonEmpty@.
--
-- Same as the standard Haskell NonEmpty, but strict.
--
-----------------------------------------------------------------------------

module Data.Strict.List.NonEmpty (
    NonEmpty(..)
  , toStrictNonEmpty
  , toLazyNonEmpty
) where

import qualified Data.List.NonEmpty as L
import Data.Strict.List
import GHC.Generics (Generic, Generic1)
import Data.Data (Data, Typeable)
import Data.Strict.Trustworthy

infixr 5 :|

-- | The strict list type.
data NonEmpty a = !a :| !(List a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a
  fromList (a : as)  = a :| fromList as
  fromList []        = error "NonEmpty.fromList: empty list"
  toList  ~(a :| as) = a : toList as

toStrictNonEmpty :: L.NonEmpty a -> NonEmpty a
toStrictNonEmpty (a L.:| as) = a :| toStrictList as

toLazyNonEmpty :: NonEmpty a -> L.NonEmpty a
toLazyNonEmpty (a :| as) = a L.:| toLazyList as
