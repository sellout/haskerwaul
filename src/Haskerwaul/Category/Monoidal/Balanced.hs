{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Balanced
  ( module Haskerwaul.Category.Monoidal.Balanced
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Braided
  ) where

import           Data.Constraint ((:-))
import           Data.Either (Either(..))
import           Data.Proxy (Proxy(..))   

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/balanced+monoidal+category)
class BraidedMonoidalCategory c t => BalancedMonoidalCategory c t where
  -- | a.k.a "twist"
  balance :: Ob c a => Proxy t -> a `c` a

instance BalancedMonoidalCategory (->) (,) where
  balance Proxy = id

instance BalancedMonoidalCategory (->) Either where
  balance Proxy = id

instance (d ~ (->), BalancedMonoidalCategory d dt) =>
         BalancedMonoidalCategory (NaturalTransformation c d) (FTensor dt) where
  balance Proxy = NT (balance (Proxy :: Proxy dt))

instance BalancedMonoidalCategory (:-) Combine where
  balance Proxy = id

instance BalancedMonoidalCategory (NaturalTransformation c (:-)) CFProd where
  balance Proxy = id
