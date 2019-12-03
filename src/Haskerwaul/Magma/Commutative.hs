{-# language UndecidableSuperClasses #-}

module Haskerwaul.Magma.Commutative
  ( module Haskerwaul.Magma.Commutative
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import Haskerwaul.Magma

-- | https://ncatlab.org/nlab/show/commutative+magma
class Magma k t a => CommutativeMagma k t a

-- commutative
--   :: (BraidedMonoidalCategory k ob t, CommutativeMagma k t a, ob a, Eq a)
--   => t a a -> Bool
-- commutative t = op t == op (braid t)

instance CommutativeMagma (->) (,) ()
