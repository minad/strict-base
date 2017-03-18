{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

infixr 5 :!

-- | The strict list type.
data List a = Nil | !a :! !(List a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance IsStrict [a] (List a) where
  toStrict   = foldr (:!) Nil
  fromStrict = foldr (:) []

instance IsList (List a) where
  type Item (List a) = a
  fromList = toStrict
  toList   = fromStrict
