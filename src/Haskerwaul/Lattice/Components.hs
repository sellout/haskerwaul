-- | This module provides the newtypes that are used to distinguish the
--   components of lattice-like structures. It also contains instances over
--   these types for classes defined in "base".
--
--  __NB__: A lot of these instances could be potentially simplified like
--          >>> instance Base.Bounded a => Base.Monoid (Meet a) where
--          >>>   mempty = Meet Base.maxBound
--          >>> instance Base.Bounded a => Base.Monoid (Join a) where
--          >>>   mempty = Meet Base.minBound
--          but we don't expect `Prelude.Bounded` instances to be lawful in any
--          way, so we avoid it.

module Haskerwaul.Lattice.Components where

import           Prelude (Integer)
import qualified Prelude as Base (Bounded(..))
import           Data.Bool
import           Data.Int
import qualified Data.Monoid as Base
import qualified Data.Ord as Base
import qualified Data.Semigroup as Base
import           Data.Word
import           Numeric.Natural

-- | The meet component of a lattice-like structure.
newtype Meet a = Meet { getMeet :: a }

-- > `Bool` instances

instance Base.Semigroup (Meet Bool) where
  Meet x <> Meet y = Meet (x && y)

instance Base.Monoid (Meet Bool) where
  mempty = Meet True

-- > `Int` instances

instance Base.Semigroup (Meet Int) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Int) where
  mempty = Meet Base.maxBound

-- > `Int8` instances

instance Base.Semigroup (Meet Int8) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Int8) where
  mempty = Meet Base.maxBound

-- > `Int16` instances

instance Base.Semigroup (Meet Int16) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Int16) where
  mempty = Meet Base.maxBound

-- > `Int32` instances

instance Base.Semigroup (Meet Int32) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Int32) where
  mempty = Meet Base.maxBound

-- > `Int64` instances

instance Base.Semigroup (Meet Int64) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Int64) where
  mempty = Meet Base.maxBound

-- > `Integer` instances

instance Base.Semigroup (Meet Integer) where
  Meet x <> Meet y = Meet (Base.min x y)

-- > `Natural` instances

instance Base.Semigroup (Meet Natural) where
  Meet x <> Meet y = Meet (Base.min x y)

-- > `()` instances

instance Base.Semigroup (Meet ()) where
  Meet () <> Meet () = Meet ()

instance Base.Monoid (Meet ()) where
  mempty = Meet ()

-- > `Word` instances

instance Base.Semigroup (Meet Word) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Word) where
  mempty = Meet Base.maxBound

-- > `Word8` instances

instance Base.Semigroup (Meet Word8) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Word8) where
  mempty = Meet Base.maxBound

-- > `Word16` instances

instance Base.Semigroup (Meet Word16) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Word16) where
  mempty = Meet Base.maxBound

-- > `Word32` instances

instance Base.Semigroup (Meet Word32) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Word32) where
  mempty = Meet Base.maxBound

-- > `Word64` instances

instance Base.Semigroup (Meet Word64) where
  Meet x <> Meet y = Meet (Base.min x y)

instance Base.Monoid (Meet Word64) where
  mempty = Meet Base.maxBound

-- | The join component of a lattice-like structure.
newtype Join a = Join { getJoin :: a }

-- > `Bool` instances

instance Base.Semigroup (Join Bool) where
  Join x <> Join y = Join (x || y)

instance Base.Monoid (Join Bool) where
  mempty = Join False

-- > `Int` instances

instance Base.Semigroup (Join Int) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Int) where
  mempty = Join Base.minBound

-- > `Int8` instances

instance Base.Semigroup (Join Int8) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Int8) where
  mempty = Join Base.minBound

-- > `Int16` instances

instance Base.Semigroup (Join Int16) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Int16) where
  mempty = Join Base.minBound

-- > `Int32` instances

instance Base.Semigroup (Join Int32) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Int32) where
  mempty = Join Base.minBound

-- > `Int64` instances

instance Base.Semigroup (Join Int64) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Int64) where
  mempty = Join Base.minBound

-- > `Integer` instances

instance Base.Semigroup (Join Integer) where
  Join x <> Join y = Join (Base.max x y)

-- > `Natural` instances

instance Base.Semigroup (Join Natural) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Natural) where
  mempty = Join 0

-- > `()` instances

instance Base.Semigroup (Join ()) where
  Join () <> Join () = Join ()

instance Base.Monoid (Join ()) where
  mempty = Join ()

-- > `Word` instances

instance Base.Semigroup (Join Word) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Word) where
  mempty = Join 0

-- > `Word8` instances

instance Base.Semigroup (Join Word8) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Word8) where
  mempty = Join 0

-- > `Word16` instances

instance Base.Semigroup (Join Word16) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Word16) where
  mempty = Join 0

-- > `Word32` instances

instance Base.Semigroup (Join Word32) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Word32) where
  mempty = Join 0

-- > `Word64` instances

instance Base.Semigroup (Join Word64) where
  Join x <> Join y = Join (Base.max x y)

instance Base.Monoid (Join Word64) where
  mempty = Join 0
