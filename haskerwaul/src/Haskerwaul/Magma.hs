{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Magma where

import qualified Control.Category as Base
import qualified Data.Bifunctor as Base
import Data.Bool (Bool)
import Data.Constraint (Dict (..), top, (:-) (..), type (&))
import Data.Either (Either (..))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Kind (Constraint)
import qualified Data.Monoid as Base
import qualified Data.Ord as Base
import qualified Data.Semigroup as Base
import qualified Data.Tuple as Base
import Data.Type.Equality ((:~:) (Refl))
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Haskerwaul.Categorification.Horizontal
import Haskerwaul.Lattice.Components
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural
import Numeric.Natural (Natural)
import Prelude (Integer)

-- | [nLab](https://ncatlab.org/nlab/show/magma)
--
--  __NB__: Since this is just any closed binary operation, it's lawless and
--          thus not worth implementing unless there are other instances to
--          implement as well. This exists to complete the diamond where other
--          type classes extend this with various laws and we need them all to
--          apply to the same operation.
class (TOb (Ob c) t, Ob c a) => Magma c t a where
  op :: t a a `c` a

instance {-# OVERLAPPABLE #-} (Base.Semigroup a) => Magma (->) (,) a where
  op = Base.uncurry (Base.<>)

instance (Magma (->) (,) a, Magma (->) (,) b) => Magma (->) (,) (a, b) where
  op ((x, y), (x', y')) = (op (x, x'), op (y, y'))

instance Magma (->) Either a where
  op = \case
    Left a -> a
    Right a -> a

instance BOb (Magma (->) (,)) (Magma (->) (,)) (Magma (->) (,)) (,) where
  inB = Sub Dict

instance Magma (->) (,) (Join Bool) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Bool) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Int) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Int) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Int8) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Int8) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Int16) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Int16) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Int32) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Int32) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Int64) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Int64) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Integer) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Integer) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Natural) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Natural) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) () where
  op ((), ()) = ()

instance Magma (->) (,) (Join ()) where
  op = Join Base.. op Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet ()) where
  op = Meet Base.. op Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Word) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Word) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Word8) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Word8) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Word16) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Word16) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Word32) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Word32) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (->) (,) (Join Word64) where
  op = Join Base.. Base.uncurry Base.max Base.. Base.bimap getJoin getJoin

instance Magma (->) (,) (Meet Word64) where
  op = Meet Base.. Base.uncurry Base.min Base.. Base.bimap getMeet getMeet

instance Magma (:-) (&) (() :: Constraint) where
  op = top

-- __NB__: These definitions belong in "Haskerwaul.Magmoid", but they’d be
--         orphans there.

-- | Just a bit of sugar over `op`, when it's used categorcally.
(.) ::
  (Magma (DinaturalTransformation (->)) Procompose c) =>
  z `c` b ->
  a `c` z ->
  a `c` b
f . g = runDT op (Procompose f g)

-- | All `Base.Category` instances are also `Haskerwaul.Magmoid.Magmoid`
--   instances.
instance
  {-# OVERLAPPABLE #-}
  (Base.Category c) =>
  Magma (DinaturalTransformation (->)) Procompose c
  where
  op = DT (\(Procompose f g) -> f Base.. g)

-- | If /C/ is a `Haskerwaul.Magmoid.Magmoid`, then so are /C/-valued
--   bifunctors.
instance
  (HorizontalCategorification Magma c) =>
  Magma (DinaturalTransformation (->)) Procompose (DinaturalTransformation c)
  where
  op = DT (\(Procompose (DT f) (DT g)) -> DT (f . g))

-- | a discrete groupoid
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/discrete+category)
instance Magma (DinaturalTransformation (->)) Procompose (:~:) where
  op = DT (\(Procompose Refl Refl) -> Refl)
