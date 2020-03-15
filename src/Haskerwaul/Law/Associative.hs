{-# language TypeApplications #-}

module Haskerwaul.Law.Associative where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

-- | The type here is a bit hard to read, but it's basically
--  @(a -> a -> `Bool`) -> a -> a -> a -> `Bool`@, but generalized to an
--   arbitrary topos.
associativeLaw :: forall c t a
                . (Ob c (t a a), SemigroupalCategory c t, Magma c t a)
               => Law c (t (t a a) a) a
associativeLaw = Law (op . first p (op @c)) (op . second p (op @c) . from assoc)
  where
    p = Proxy :: Proxy c
