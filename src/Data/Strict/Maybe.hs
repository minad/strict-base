{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Maybe
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict @Maybe@.
--
-- Same as the standard Haskell @Maybe@, but @Just _|_ = _|_@
--
-- Note that strict @Maybe@ is not a monad since
-- @ return _|_ >>= f = _|_ @
-- which is not necessarily the same as @f _|_@.
--
-----------------------------------------------------------------------------

module Data.Strict.Maybe (
    Maybe(..)
  , isJust
  , isNothing
  , fromMaybe
  , maybe
  , listToMaybe
  , maybeToList
  , mapMaybe
  , catMaybes
) where

import qualified Data.Maybe as L
import Prelude hiding (Maybe(..), maybe)
import Data.Semigroup (Semigroup(..))
import GHC.Generics (Generic, Generic1)
import Data.Data (Data, Typeable)
import Data.Strict.Class

-- | The type of strict optional values.
data Maybe a = Nothing | Just !a
  deriving (Eq, Ord, Show, Read, Functor, Foldable, Traversable, Generic, Generic1, Data, Typeable)

instance IsStrict (L.Maybe a) (Maybe a) where
  toStrict   L.Nothing  = Nothing
  toStrict   (L.Just x) = Just x
  fromStrict Nothing    = L.Nothing
  fromStrict (Just x)   = L.Just x

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> m       = m
  m       <> Nothing = m
  Just x1 <> Just x2 = Just (x1 <> x2)

instance Monoid a => Monoid (Maybe a) where
  mempty                    = Nothing
  Nothing `mappend` m       = m
  m       `mappend` Nothing = m
  Just x1 `mappend` Just x2 = Just (x1 `mappend` x2)

-- | Yields 'True' iff the argument is of the form @Just _@.
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _       = True

-- | Yields 'True' iff the argument is 'Nothing'.
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

-- | Given a default value and a 'Maybe', yield the default value if the
-- 'Maybe' argument is 'Nothing' and extract the value out of the 'Just'
-- otherwise.
fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just y) = y

-- | Given a default value, a function and a 'Maybe' value, yields the default
-- value if the 'Maybe' value is 'Nothing' and applies the function to the
-- value stored in the 'Just' otherwise.
maybe :: b -> (a -> b) -> Maybe a -> b
maybe x _ Nothing  = x
maybe _ f (Just y) = f y

-- | Analogous to 'L.listToMaybe' in "Data.Maybe".
listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (a:_) = Just a

-- | Analogous to 'L.maybeToList' in "Data.Maybe".
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

-- | Analogous to 'L.catMaybes' in "Data.Maybe".
catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

-- | Analogous to 'L.mapMaybe' in "Data.Maybe".
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ []     = []
mapMaybe f (x:xs) =
  case f x of
    Nothing -> rs
    Just r  -> r:rs
  where rs = mapMaybe f xs
