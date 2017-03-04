-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Strict
-- Copyright   :  (c) 2006-2007 Roman Leshchinskiy
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Stability   :  experimental
-- Portability :  portable
--
-- Strict versions of some standard Haskell types.
--
-----------------------------------------------------------------------------

module Data.Strict (
    module Data.Strict.Class
  , module Data.Strict.Tuple
  , module Data.Strict.Maybe
  , module Data.Strict.Either
  , module Data.Strict.List
  , module Data.Strict.List.NonEmpty
) where

import Data.Strict.Class
import Data.Strict.Tuple
import Data.Strict.Maybe
import Data.Strict.Either
import Data.Strict.List
import Data.Strict.List.NonEmpty
