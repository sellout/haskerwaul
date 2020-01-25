-- | This module provides the newtypes that are used to distinguish the
--   components of semiring-like structures. It also contains instances over
--   these types for classes defined in "base".
--
--  __NB__: A lot of these instances could be potentially simplified like
--          >>> instance Base.Num a => Base.Semigroup (Additive a) where
--          >>>   Add x <> Add y = Add (x Base.+ y)
--          >>> instance Base.Num a => Base.Semigroup (Multiplicative a) where
--          >>>   Multiply x <> Multiply y = Multiply (x Base.* y)
--          but we don't expect `Prelude.Num` instances to be lawful in any way,
--          so we avoid it.
module Haskerwaul.Semiring.Components where

import           Prelude (Integer)
import qualified Prelude as Base (Num(..))
import           Data.Bool (Bool(..))
import qualified Data.Bool as Base
import           Data.Int
import qualified Data.Monoid as Base
import qualified Data.Semigroup as Base
import           Data.Word
import           Numeric.Natural

import Haskerwaul.Lattice.Components

-- | The additive component of a semiring-like structure.
newtype Additive a = Add { sum :: a }

-- > Bool instances

instance Base.Semigroup (Additive Bool) where
  Add x <> Add y = Add ((x Base.|| y) Base.&& Base.not (x Base.&& y))

instance Base.Monoid (Additive Bool) where
  mempty = Add False

-- > Int instances

instance Base.Semigroup (Additive Int) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Int) where
  mempty = Add 0

-- > Int8 instances

instance Base.Semigroup (Additive Int8) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Int8) where
  mempty = Add 0

-- > Int16 instances

instance Base.Semigroup (Additive Int16) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Int16) where
  mempty = Add 0

-- > Int32 instances

instance Base.Semigroup (Additive Int32) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Int32) where
  mempty = Add 0

-- > Int64 instances

instance Base.Semigroup (Additive Int64) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Int64) where
  mempty = Add 0

-- > Integer instances

instance Base.Semigroup (Additive Integer) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Integer) where
  mempty = Add 0

-- > Natural instances

instance Base.Semigroup (Additive Natural) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Natural) where
  mempty = Add 0

instance Base.Semigroup (Additive (Join Natural)) where
  Add x <> Add y = Add (x Base.<> y)

instance Base.Monoid (Additive (Join Natural)) where
  mempty = Add Base.mempty

-- > () instances

instance Base.Semigroup (Additive ()) where
  Add () <> Add () = Add ()

instance Base.Monoid (Additive ()) where
  mempty = Add ()

-- > Word instances

instance Base.Semigroup (Additive Word) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Word) where
  mempty = Add 0

-- > Word8 instances

instance Base.Semigroup (Additive Word8) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Word8) where
  mempty = Add 0

-- > Word16 instances

instance Base.Semigroup (Additive Word16) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Word16) where
  mempty = Add 0

-- > Word32 instances

instance Base.Semigroup (Additive Word32) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Word32) where
  mempty = Add 0

-- > Word64 instances

instance Base.Semigroup (Additive Word64) where
  Add x <> Add y = Add (x Base.+ y)

instance Base.Monoid (Additive Word64) where
  mempty = Add 0

-- | The multiplicative component of a semiring-like structure.
newtype Multiplicative a = Multiply { product :: a }

-- > Bool instances

instance Base.Semigroup (Multiplicative Bool) where
  Multiply x <> Multiply y = Multiply (x Base.&& y)

instance Base.Monoid (Multiplicative Bool) where
  mempty = Multiply True

-- > Int instances

instance Base.Semigroup (Multiplicative Int) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Int) where
  mempty = Multiply 1

-- > Int8 instances

instance Base.Semigroup (Multiplicative Int8) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Int8) where
  mempty = Multiply 1

-- > Int16 instances

instance Base.Semigroup (Multiplicative Int16) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Int16) where
  mempty = Multiply 1

-- > Int32 instances

instance Base.Semigroup (Multiplicative Int32) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Int32) where
  mempty = Multiply 1

-- > Int64 instances

instance Base.Semigroup (Multiplicative Int64) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Int64) where
  mempty = Multiply 1

-- > Integer instances

instance Base.Semigroup (Multiplicative Integer) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Integer) where
  mempty = Multiply 1

-- > Natural instances

instance Base.Semigroup (Multiplicative Natural) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Natural) where
  mempty = Multiply 1

instance Base.Semigroup (Multiplicative (Join Natural)) where
  Multiply (Join x) <> Multiply (Join y) = Multiply (Join (x Base.+ y))

instance Base.Monoid (Multiplicative (Join Natural)) where
  mempty = Multiply (Join 0)

-- > () instances

instance Base.Semigroup (Multiplicative ()) where
  Multiply () <> Multiply () = Multiply ()

instance Base.Monoid (Multiplicative ()) where
  mempty = Multiply ()

-- > Word instances

instance Base.Semigroup (Multiplicative Word) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Word) where
  mempty = Multiply 1

-- > Word8 instances

instance Base.Semigroup (Multiplicative Word8) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Word8) where
  mempty = Multiply 1

-- > Word16 instances

instance Base.Semigroup (Multiplicative Word16) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Word16) where
  mempty = Multiply 1

-- > Word32 instances

instance Base.Semigroup (Multiplicative Word32) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Word32) where
  mempty = Multiply 1

-- > Word64 instances

instance Base.Semigroup (Multiplicative Word64) where
  Multiply x <> Multiply y = Multiply (x Base.* y)

instance Base.Monoid (Multiplicative Word64) where
  mempty = Multiply 1
