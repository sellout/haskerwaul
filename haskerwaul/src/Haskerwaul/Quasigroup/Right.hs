{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Right
  ( module Haskerwaul.Quasigroup.Right,

    -- * extended modules
    module Haskerwaul.Magma,
  )
where

import Data.Either (Either, either)
import Data.Int (Int)
import Data.Type.Equality ((:~:) (Refl))
import Data.Void (Void, absurd)
import Haskerwaul.Magma
import Haskerwaul.Semiring.Components
import Haskerwaul.Transformation.Dinatural
import Prelude (Integer)
import qualified Prelude as Base (Num (..))

-- | [nLab](https://ncatlab.org/nlab/show/quasigroup#definitions)
--
-- = laws
--   [right quotient 1]: @`op` x (`rightQuotient` x y) == y@
--   [right quotient 2]: @`rightQuotient` x (`op` x y) == y@
class (Magma c t a) => RightQuasigroup c t a where
  rightQuotient :: t a a `c` a

instance RightQuasigroup (->) Either Void where
  rightQuotient = either absurd absurd

instance RightQuasigroup (->) (,) () where
  rightQuotient ((), ()) = ()

-- instance RightQuasigroup (DinaturalTransformation (->)) Procompose c =>
--          RightQuasigroup (DinaturalTransformation (->)) Procompose (Op c) where
--   inverse = DT (\(Op f) -> Op (runDT inverse f))

instance RightQuasigroup (->) (,) (Additive Int) where
  rightQuotient (Add x, Add y) = Add (x Base.- y)

instance RightQuasigroup (->) (,) (Additive Integer) where
  rightQuotient (Add x, Add y) = Add (x Base.- y)

-- __NB__: These definitions belong in "Haskerwaul.Quasigroupoid", but they’d be
--         orphans there.

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance RightQuasigroup (DinaturalTransformation (->)) Procompose (:~:) where
  rightQuotient = DT (\(Procompose Refl Refl) -> Refl)
