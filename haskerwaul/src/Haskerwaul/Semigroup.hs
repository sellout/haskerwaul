{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Semigroup
  ( module Haskerwaul.Semigroup,

    -- * extended modules
    module Haskerwaul.Magma.Flexible,
  )
where

import qualified Control.Category as Base
import Data.Bool (Bool)
import Data.Constraint (Dict (..), (:-) (..))
import Data.Either (Either)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Kind (Constraint)
import qualified Data.Semigroup as Base
import Data.Type.Equality ((:~:))
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Constraint
import Haskerwaul.Lattice.Components
import Haskerwaul.Magma.Flexible
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural
import Numeric.Natural (Natural)
import Prelude (Integer)

-- | [nLab](https://ncatlab.org/nlab/show/semigroup)
--
-- = laws
--   [`Haskerwaul.Law.Associativity.associativity`]: @`op` x (`op` y z) == `op` (`op` x y) z
class (FlexibleMagma c t a) => Semigroup c t a

-- | Take advantage of this instance when possible. I.e., define your semigroup
--   using `Base.Semigroup` whenever possible.
instance {-# OVERLAPPABLE #-} (Base.Semigroup a) => Semigroup (->) (,) a

instance (Semigroup (->) (,) a, Semigroup (->) (,) b) => Semigroup (->) (,) (a, b)

instance Semigroup (->) Either a

instance BOb (Semigroup (->) (,)) (Semigroup (->) (,)) (Semigroup (->) (,)) (,) where
  inB = Sub Dict

instance Semigroup (->) (,) (Join Bool)

instance Semigroup (->) (,) (Meet Bool)

instance Semigroup (->) (,) (Join Int)

instance Semigroup (->) (,) (Meet Int)

instance Semigroup (->) (,) (Join Int8)

instance Semigroup (->) (,) (Meet Int8)

instance Semigroup (->) (,) (Join Int16)

instance Semigroup (->) (,) (Meet Int16)

instance Semigroup (->) (,) (Join Int32)

instance Semigroup (->) (,) (Meet Int32)

instance Semigroup (->) (,) (Join Int64)

instance Semigroup (->) (,) (Meet Int64)

instance Semigroup (->) (,) (Join Integer)

instance Semigroup (->) (,) (Meet Integer)

instance Semigroup (->) (,) (Join Natural)

instance Semigroup (->) (,) (Meet Natural)

instance Semigroup (->) (,) ()

instance Semigroup (->) (,) (Join ())

instance Semigroup (->) (,) (Meet ())

instance Semigroup (->) (,) (Join Word)

instance Semigroup (->) (,) (Meet Word)

instance Semigroup (->) (,) (Join Word8)

instance Semigroup (->) (,) (Meet Word8)

instance Semigroup (->) (,) (Join Word16)

instance Semigroup (->) (,) (Meet Word16)

instance Semigroup (->) (,) (Join Word32)

instance Semigroup (->) (,) (Meet Word32)

instance Semigroup (->) (,) (Join Word64)

instance Semigroup (->) (,) (Meet Word64)

instance Semigroup (:-) Combine (() :: Constraint)

-- __NB__: These definitions belong in "Haskerwaul.Semicategory", but they’d be
--         orphans there.

-- | All `Base.Category` instances are also
--  `Haskerwaul.Semicategory.Semicategory` instances.
instance
  {-# OVERLAPPABLE #-}
  (Base.Category c) =>
  Semigroup (DinaturalTransformation (->)) Procompose c

-- | If /C/ is a `Haskerwaul.Semicategory.Semicategory`, then so are /C/-valued
--   bifunctors.
instance
  (HorizontalCategorification Semigroup c) =>
  Semigroup
    (DinaturalTransformation (->))
    Procompose
    (DinaturalTransformation c)

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance Semigroup (DinaturalTransformation (->)) Procompose (:~:)
