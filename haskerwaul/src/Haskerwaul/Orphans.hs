{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module holds instances that create an import cycle when put in the
--   obvious places. Moving instances out of here is a win.
module Haskerwaul.Orphans where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Opposite
import Haskerwaul.Functor
import Haskerwaul.Functor.Adjoint as Adj
import Haskerwaul.Transformation.Natural

instance
  (c ~ (->), d ~ (->), AdjointFunctor c d l r) =>
  Magma (NaturalTransformation d c) Compose (Compose r l)
  where
  op =
    NT
      ( Compose
          . map @d (Adj.epsilon (Proxy :: Proxy c))
          . getCompose
          . map @_ @d getCompose
          . getCompose
      )

instance
  (c ~ (->), d ~ (->), AdjointFunctor c d l r) =>
  Magma (Opposite (NaturalTransformation d c)) Compose (Compose l r)
  where
  op =
    Opposite
      ( NT
          ( Compose
              . map @_ @c Compose
              . Compose
              . map @c (eta (Proxy :: Proxy d))
              . getCompose
          )
      )

-- instance {-# overlapping #-} (c ~ (->), d ~ (->), AdjointFunctor c d l r) =>
--          UnitalMagma (NaturalTransformation d c) Compose (Compose r l) where
--   unit Proxy = NT (Compose . eta (Proxy :: Proxy d) . runIdentity)

instance
  (c ~ (->), d ~ (->), AdjointFunctor c d l r) =>
  UnitalMagma (Opposite (NaturalTransformation c d)) Compose (Compose l r)
  where
  unit Proxy = Opposite (NT (Identity . Adj.epsilon (Proxy :: Proxy c) . getCompose))

-- -- * Every object in a `CartesianMonoidalCategory` is a `Comonoid` in a unique way.

-- instance (CartesianMonoidalCategory c, t ~ Prod c, Ob c a) =>
--          Magma (Opposite c) t a where
--   op = Opposite diagonal

-- instance (CartesianMonoidalCategory c, t ~ Prod c, Ob c a) =>
--          Semigroup (Opposite c) t a

-- instance (CartesianMonoidalCategory c, t ~ Prod c, Ob c a) =>
--          UnitalMagma (Opposite c) t a where
--   unit Proxy = Opposite (!)
