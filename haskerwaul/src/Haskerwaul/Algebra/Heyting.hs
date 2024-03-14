{-# language UndecidableSuperClasses #-}

module Haskerwaul.Algebra.Heyting
  ( module Haskerwaul.Algebra.Heyting
  -- * extended modules
  , module Haskerwaul.Lattice.Distributive.Bounded
  ) where

import qualified Prelude as Base (Bounded(..))
import qualified Data.Bool as Base
import qualified Data.Int as Base
import qualified Data.Ord as Base
import qualified Data.Word as Base

import Haskerwaul.Lattice.Distributive.Bounded

-- | [nLab](https://ncatlab.org/nlab/show/Heyting+algebra)
class BoundedDistributiveLattice c t a => HeytingAlgebra c t a where
  implies :: t a a `c` a

-- | A common definition of implication for totally-ordered sets. It's possible
--   that we could just define a single instance with these constraints from
--   @base@, but I don't trust the instances for those classes in general, so we
--   create individual instances explicitly.
implies' :: (Base.Bounded a, Base.Ord a) => (a, a) -> a
implies' (x, y) = if x Base.<= y then Base.maxBound else y

instance HeytingAlgebra (->) (,) Base.Bool where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Int where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Int8 where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Int16 where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Int32 where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Int64 where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Word where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Word8 where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Word16 where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Word32 where
  implies = implies'

instance HeytingAlgebra (->) (,) Base.Word64 where
  implies = implies'
