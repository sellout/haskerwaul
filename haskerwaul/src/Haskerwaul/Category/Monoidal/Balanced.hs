{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
-- __NB__: GHC claims that the constraints on the default impl of `balance` are
--         redundant, but symmetry law is needed for that property to hold.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Haskerwaul.Category.Monoidal.Balanced
  ( module Haskerwaul.Category.Monoidal.Balanced,

    -- * extended modules
    module Haskerwaul.Category.Monoidal.Braided,
  )
where

import Data.Constraint ((:-))
import Data.Either (Either (..))
import Data.Proxy (Proxy (..))
import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Monoidal.Braided
import {-# SOURCE #-} Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/balanced+monoidal+category)
class (BraidedMonoidalCategory c t) => BalancedMonoidalCategory c t where
  -- | a.k.a "twist"
  balance :: (Ob c a) => Proxy t -> a `c` a

  -- | "Every symmetric monoidal category is balanced in a canonical way ..."
  --   ⸻[nLab](https://ncatlab.org/nlab/show/balanced+monoidal+category#properties)
  default balance :: (SymmetricMonoidalCategory c t, Ob c a) => Proxy t -> a `c` a
  balance Proxy = id

instance BalancedMonoidalCategory (->) (,) where
  balance Proxy = id

instance BalancedMonoidalCategory (->) Either where
  balance Proxy = id

instance
  (d ~ (->), dt ~ (,), BalancedMonoidalCategory d dt) =>
  BalancedMonoidalCategory (NaturalTransformation c d) (FTensor dt)
  where
  balance Proxy = NT (balance (Proxy :: Proxy dt))

instance BalancedMonoidalCategory (:-) Combine where
  balance Proxy = id

instance BalancedMonoidalCategory (NaturalTransformation c (:-)) CFProd where
  balance Proxy = id

instance
  (d ~ (->), BalancedMonoidalCategory d dt) =>
  BalancedMonoidalCategory (DinaturalTransformation d) (BTensor dt)
  where
  balance Proxy = DT (balance (Proxy :: Proxy dt))
