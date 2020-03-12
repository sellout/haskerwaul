{-# language UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Left
  ( module Haskerwaul.Quasigroup.Left
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import           Prelude (Integer)
import qualified Prelude as Base (Num(..))
import           Data.Int (Int)

import Haskerwaul.Semiring.Components
import Haskerwaul.Magma

-- | [nLab](https://ncatlab.org/nlab/show/quasigroup#definitions)
class Magma c t a => LeftQuasigroup c t a where
  leftQuotient :: t a a `c` a

-- instance LeftQuasigroup (->) Either Void where
--   leftQuotient = \case

instance LeftQuasigroup (->) (,) () where
  leftQuotient ((), ()) = ()

-- instance Magma (NaturalTransformation2 (->)) CProd c =>
--          LeftQuasigroup (NaturalTransformation2 (->)) CProd (Iso c) where
--   inverse = NT2 (\(Iso f g) -> Iso g f)

-- instance LeftQuasigroup (NaturalTransformation2 (->)) CProd c =>
--          LeftQuasigroup (NaturalTransformation2 (->)) CProd (Op c) where
--   inverse = NT2 (\(Op f) -> Op (runNT2 inverse f))

instance LeftQuasigroup (->) (,) (Additive Int) where
  leftQuotient (Add x, Add y) = Add (y Base.- x)

instance LeftQuasigroup (->) (,) (Additive Integer) where
  leftQuotient (Add x, Add y) = Add (y Base.- x)
