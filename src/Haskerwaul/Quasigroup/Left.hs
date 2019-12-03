{-# language UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Left
  ( module Haskerwaul.Quasigroup.Left
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import Haskerwaul.Bifunctor
import Haskerwaul.Semigroupoid
import Haskerwaul.Magma
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | https://ncatlab.org/nlab/show/quasigroup#definitions
class Magma k t a => LeftQuasigroup k t a where
  leftQuotient :: t a a `k` a

-- instance LeftQuasigroup (->) Either Void where
--   leftQuotient = \case

instance LeftQuasigroup (->) (,) () where
  leftQuotient ((), ()) = ()

-- instance Magma (NaturalTransformation2 (->)) CProd k =>
--          LeftQuasigroup (NaturalTransformation2 (->)) CProd (Iso k) where
--   inverse = NT2 (\(Iso f g) -> Iso g f)

-- instance LeftQuasigroup (NaturalTransformation2 (->)) CProd k =>
--          LeftQuasigroup (NaturalTransformation2 (->)) CProd (Op k) where
--   inverse = NT2 (\(Op f) -> Op (runNT2 inverse f))

instance LeftQuasigroup (NaturalTransformation2 (->)) CProd k =>
         LeftQuasigroup
         (NaturalTransformation2 (->))
         CProd
         (FullSubcategory c k) where
  leftQuotient = NT2 FS . leftQuotient . bimap (NT2 inclusion) (NT2 inclusion)
