{-# language UndecidableSuperClasses #-}

module Haskerwaul.Quasigroup.Right
  ( module Haskerwaul.Quasigroup.Right
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import           Prelude (Integer)
import qualified Prelude as Base (Num(..))
import           Data.Either (Either, either)
import           Data.Int (Int)
import           Data.Void (Void, absurd)

import Haskerwaul.Magma
import Haskerwaul.Semiring.Components

-- | [nLab](https://ncatlab.org/nlab/show/quasigroup#definitions)
--
-- = laws
--   [right quotient 1]: @`op` x (`rightQuotient` x y) == y@
--   [right quotient 2]: @`rightQuotient` x (`op` x y) == y@
class Magma c t a => RightQuasigroup c t a where
  rightQuotient :: t a a `c` a

instance RightQuasigroup (->) Either Void where
  rightQuotient = either absurd absurd

instance RightQuasigroup (->) (,) () where
  rightQuotient ((), ()) = ()

-- instance RightQuasigroup (NaturalTransformation2 (->)) CProd c =>
--          RightQuasigroup (NaturalTransformation2 (->)) CProd (Op c) where
--   inverse = NT2 (\(Op f) -> Op (runNT2 inverse f))

instance RightQuasigroup (->) (,) (Additive Int) where
  rightQuotient (Add x, Add y) = Add (x Base.- y)

instance RightQuasigroup (->) (,) (Additive Integer) where
  rightQuotient (Add x, Add y) = Add (x Base.- y)
