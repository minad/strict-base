{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.List.NonEmpty
-- Copyright   :  (c) 2017 Daniel Mendler
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Daniel Mendler <mail@daniel-mendler.de>
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
) where

import qualified Data.List.NonEmpty as L
import Data.Strict.List
import GHC.Generics (Generic, Generic1)
import Data.Data (Data, Typeable)
import Data.Strict.Trustworthy
import Data.Strict.Class

infixr 5 :|

-- | The strict list type.
data NonEmpty a = !a :| !(List a)
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance IsStrict (L.NonEmpty a) (NonEmpty a) where
  toStrict   (a L.:| as) = a   :| toStrict as
  fromStrict (a   :| as) = a L.:| fromStrict as

instance IsList (NonEmpty a) where
  type Item (NonEmpty a) = a
  fromList (a : as)  = a :| fromList as
  fromList []        = error "NonEmpty.fromList: empty list"
  toList  ~(a :| as) = a : toList as
