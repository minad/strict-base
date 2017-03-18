{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict.Class
-- Copyright   :  (c) 2017 Daniel Mendler
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Daniel Mendler <mail@daniel-mendler.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- IsStrict isomorphism for conversion.
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
