{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Class
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

module Data.Strict.Class (
  IsStrict(..),
  liftStrict
) where

class IsStrict l s | l -> s, s -> l where
  fromStrict :: s -> l
  toStrict   :: l -> s

liftStrict :: IsStrict l s => (l -> l) -> (s -> s)
liftStrict f = toStrict . f . fromStrict
