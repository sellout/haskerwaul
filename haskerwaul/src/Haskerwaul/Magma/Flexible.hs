{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Magma.Flexible
  ( module Haskerwaul.Magma.Flexible,

    -- * extended modules
    module Haskerwaul.Magma,
  )
where

import qualified Control.Category as Base
import Data.Bool (Bool)
import Data.Constraint ((:-) (..))
import Data.Either (Either)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Kind (Constraint)
import qualified Data.Semigroup as Base
import Data.Type.Equality ((:~:))
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Constraint
import Haskerwaul.Lattice.Components
import Haskerwaul.Magma
import Haskerwaul.Semiring.Components
import Haskerwaul.Transformation.Dinatural
import Numeric.Natural (Natural)
import Prelude (Integer)

-- |
--
-- = resources
--
-- - [nLab](https://ncatlab.org/nlab/show/flexible+magma)
--
-- = laws
--
--   [`Haskerwaul.Law.Flexibility.flexibility`]: @`op` (`op` x y) x == `op` x (`op` y x)@
class (Magma c t a) => FlexibleMagma c t a

instance {-# OVERLAPPABLE #-} (Base.Semigroup a) => FlexibleMagma (->) (,) a

instance
  (FlexibleMagma (->) (,) a, FlexibleMagma (->) (,) b) =>
  FlexibleMagma (->) (,) (a, b)

instance FlexibleMagma (:-) Combine (() :: Constraint)

instance FlexibleMagma (->) Either a

instance FlexibleMagma (->) (,) (Join Bool)

instance FlexibleMagma (->) (,) (Meet Bool)

instance FlexibleMagma (->) (,) (Additive Int)

instance FlexibleMagma (->) (,) (Join Int)

instance FlexibleMagma (->) (,) (Meet Int)

instance FlexibleMagma (->) (,) (Multiplicative Int)

instance FlexibleMagma (->) (,) (Additive Int8)

instance FlexibleMagma (->) (,) (Join Int8)

instance FlexibleMagma (->) (,) (Meet Int8)

instance FlexibleMagma (->) (,) (Multiplicative Int8)

instance FlexibleMagma (->) (,) (Additive Int16)

instance FlexibleMagma (->) (,) (Join Int16)

instance FlexibleMagma (->) (,) (Meet Int16)

instance FlexibleMagma (->) (,) (Multiplicative Int16)

instance FlexibleMagma (->) (,) (Additive Int32)

instance FlexibleMagma (->) (,) (Join Int32)

instance FlexibleMagma (->) (,) (Meet Int32)

instance FlexibleMagma (->) (,) (Multiplicative Int32)

instance FlexibleMagma (->) (,) (Additive Int64)

instance FlexibleMagma (->) (,) (Join Int64)

instance FlexibleMagma (->) (,) (Meet Int64)

instance FlexibleMagma (->) (,) (Multiplicative Int64)

instance FlexibleMagma (->) (,) (Additive Integer)

instance FlexibleMagma (->) (,) (Join Integer)

instance FlexibleMagma (->) (,) (Meet Integer)

instance FlexibleMagma (->) (,) (Multiplicative Integer)

instance FlexibleMagma (->) (,) (Additive Natural)

instance FlexibleMagma (->) (,) (Join Natural)

instance FlexibleMagma (->) (,) (Meet Natural)

instance FlexibleMagma (->) (,) (Multiplicative Natural)

instance FlexibleMagma (->) (,) (Additive (Join Natural))

instance FlexibleMagma (->) (,) (Multiplicative (Join Natural))

instance FlexibleMagma (->) (,) ()

instance FlexibleMagma (->) (,) (Additive ())

instance FlexibleMagma (->) (,) (Join ())

instance FlexibleMagma (->) (,) (Meet ())

instance FlexibleMagma (->) (,) (Multiplicative ())

instance FlexibleMagma (->) (,) (Additive Word)

instance FlexibleMagma (->) (,) (Join Word)

instance FlexibleMagma (->) (,) (Meet Word)

instance FlexibleMagma (->) (,) (Multiplicative Word)

instance FlexibleMagma (->) (,) (Additive Word8)

instance FlexibleMagma (->) (,) (Join Word8)

instance FlexibleMagma (->) (,) (Meet Word8)

instance FlexibleMagma (->) (,) (Multiplicative Word8)

instance FlexibleMagma (->) (,) (Additive Word16)

instance FlexibleMagma (->) (,) (Join Word16)

instance FlexibleMagma (->) (,) (Meet Word16)

instance FlexibleMagma (->) (,) (Multiplicative Word16)

instance FlexibleMagma (->) (,) (Additive Word32)

instance FlexibleMagma (->) (,) (Join Word32)

instance FlexibleMagma (->) (,) (Meet Word32)

instance FlexibleMagma (->) (,) (Multiplicative Word32)

instance FlexibleMagma (->) (,) (Additive Word64)

instance FlexibleMagma (->) (,) (Join Word64)

instance FlexibleMagma (->) (,) (Meet Word64)

instance FlexibleMagma (->) (,) (Multiplicative Word64)

-- __NB__: These definitions belong in "Haskerwaul.Magmoid.Flexible", but they’d
--         be orphans there.

-- | All `Base.Category` instances are also
--  `Haskerwaul.Magmoid.Flexible.FlexibleMagmoid` instances.
instance
  {-# OVERLAPPABLE #-}
  (Base.Category c) =>
  FlexibleMagma (DinaturalTransformation (->)) Procompose c

-- | If /C/ is a `Haskerwaul.Magmoid.Flexible.FlexibleMagmoid`, then so are
--  /C/-valued bifunctors.
instance
  (HorizontalCategorification FlexibleMagma c) =>
  FlexibleMagma
    (DinaturalTransformation (->))
    Procompose
    (DinaturalTransformation c)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance FlexibleMagma (DinaturalTransformation (->)) Procompose (:~:)
