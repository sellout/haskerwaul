{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Left
  ( module Haskerwaul.Quasigroup.Left,

    -- * extended modules
    module Haskerwaul.Magma,
  )
where

import Data.Either (Either)
import qualified Data.Either as Base
import Data.Int (Int)
import Data.Void (Void)
import qualified Data.Void as Base
import Haskerwaul.Magma
import Haskerwaul.Semiring.Components
import Prelude (Integer)
import qualified Prelude as Base (Num (..))

-- | [nLab](https://ncatlab.org/nlab/show/quasigroup#definitions)
--
-- = laws
--   [left quotient 1]: @`op` (`leftQuotient` x y) y == x@
--   [left quotient 2]: @`leftQuotient` (`op` x y) y == x@
class (Magma c t a) => LeftQuasigroup c t a where
  leftQuotient :: t a a `c` a

instance LeftQuasigroup (->) Either Void where
  leftQuotient = Base.either Base.absurd Base.absurd

instance LeftQuasigroup (->) (,) () where
  leftQuotient ((), ()) = ()

-- instance Magmoid c =>
--          LeftQuasigroup (DinaturalTransformation (->)) Procompose (Iso c) where
--   inverse = DT (\(Iso f g) -> Iso g f)

-- instance LeftQuasigroup (DinaturalTransformation (->)) Procompose c =>
--          LeftQuasigroup (DinaturalTransformation (->)) Procompose (Op c) where
--   inverse = DT (\(Op f) -> Op (runDT inverse f))

instance LeftQuasigroup (->) (,) (Additive Int) where
  leftQuotient (Add x, Add y) = Add (y Base.- x)

instance LeftQuasigroup (->) (,) (Additive Integer) where
  leftQuotient (Add x, Add y) = Add (y Base.- x)
