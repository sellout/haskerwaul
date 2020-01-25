{-# language UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Right
  ( module Haskerwaul.Quasigroup.Right
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import           Prelude (Integer)
import qualified Prelude as Base (Num(..))
import           Data.Int (Int)

import Haskerwaul.Bifunctor
import Haskerwaul.Magma
import Haskerwaul.Semigroupoid
import Haskerwaul.Semiring.Components
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | https://ncatlab.org/nlab/show/quasigroup#definitions
class Magma k t a => RightQuasigroup k t a where
  rightQuotient :: t a a `k` a

-- instance RightQuasigroup (->) Either Void where
--   rightQuotient = \case

instance RightQuasigroup (->) (,) () where
  rightQuotient ((), ()) = ()

-- instance RightQuasigroup (NaturalTransformation2 (->)) CProd k =>
--          RightQuasigroup (NaturalTransformation2 (->)) CProd (Op k) where
--   inverse = NT2 (\(Op f) -> Op (runNT2 inverse f))

instance RightQuasigroup (NaturalTransformation2 (->)) CProd k =>
         RightQuasigroup
         (NaturalTransformation2 (->))
         CProd
         (FullSubcategory c k) where
  rightQuotient = NT2 FS . rightQuotient . bimap (NT2 inclusion) (NT2 inclusion)

instance RightQuasigroup (->) (,) (Additive Int) where
  rightQuotient (Add x, Add y) = Add (x Base.- y)

instance RightQuasigroup (->) (,) (Additive Integer) where
  rightQuotient (Add x, Add y) = Add (x Base.- y)
