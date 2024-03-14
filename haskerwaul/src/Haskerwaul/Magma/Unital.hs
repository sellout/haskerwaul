{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Magma.Unital
  ( module Haskerwaul.Magma.Unital,

    -- * extended modules
    module Haskerwaul.Category.Monoidal',
    module Haskerwaul.Magma,
  )
where

import Data.Bool (Bool (..))
import Data.Constraint (Dict (..), refl, (:-) (..))
import Data.Either (Either)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Kind (Constraint)
import qualified Data.Monoid as Base
import Data.Proxy (Proxy (..))
import qualified Data.Void as Base
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Haskerwaul.Category.Monoidal'
import Haskerwaul.Constraint
import Haskerwaul.Lattice.Components
import Haskerwaul.Magma
import Haskerwaul.Object
import Haskerwaul.Semiring.Components
import Numeric.Natural (Natural)
import Prelude (Bounded (..), Integer)

-- | [nLab](https://ncatlab.org/nlab/show/unital+magma)
--
-- = laws
--   [left identity]: @`op` `unit` x == x@
--   [right identity]: @`op` x `unit` == x@
class (MonoidalCategory' c t, Magma c t a) => UnitalMagma c t a where
  unit :: Proxy t -> Unit c t `c` a

instance {-# OVERLAPPABLE #-} (Base.Monoid a) => UnitalMagma (->) (,) a where
  unit Proxy () = Base.mempty

instance
  (UnitalMagma (->) (,) a, UnitalMagma (->) (,) b) =>
  UnitalMagma (->) (,) (a, b)
  where
  unit t u = (unit t u, unit t u)

instance UnitalMagma (->) Either a where
  unit Proxy = Base.absurd

instance BOb (UnitalMagma (->) (,)) (UnitalMagma (->) (,)) (UnitalMagma (->) (,)) (,) where
  inB = Sub Dict

instance UnitalMagma (->) (,) (Join Bool) where
  unit Proxy () = Join False

instance UnitalMagma (->) (,) (Meet Bool) where
  unit Proxy () = Meet True

instance UnitalMagma (->) (,) (Additive Int) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Int) where
  unit Proxy () = Join minBound

instance UnitalMagma (->) (,) (Meet Int) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Int) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Int8) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Int8) where
  unit Proxy () = Join minBound

instance UnitalMagma (->) (,) (Meet Int8) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Int8) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Int16) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Int16) where
  unit Proxy () = Join minBound

instance UnitalMagma (->) (,) (Meet Int16) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Int16) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Int32) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Int32) where
  unit Proxy () = Join minBound

instance UnitalMagma (->) (,) (Meet Int32) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Int32) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Int64) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Int64) where
  unit Proxy () = Join minBound

instance UnitalMagma (->) (,) (Meet Int64) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Int64) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Integer) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Multiplicative Integer) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Natural) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Natural) where
  unit Proxy () = Join 0

instance UnitalMagma (->) (,) (Multiplicative Natural) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive (Join Natural)) where
  unit Proxy () = Add (Join 0)

instance UnitalMagma (->) (,) (Multiplicative (Join Natural)) where
  unit Proxy () = Multiply (Join 0)

instance UnitalMagma (->) (,) () where
  unit Proxy () = ()

instance UnitalMagma (->) (,) (Additive ()) where
  unit Proxy () = Add ()

instance UnitalMagma (->) (,) (Join ()) where
  unit Proxy () = Join ()

instance UnitalMagma (->) (,) (Meet ()) where
  unit Proxy () = Meet ()

instance UnitalMagma (->) (,) (Multiplicative ()) where
  unit Proxy () = Multiply ()

instance UnitalMagma (->) (,) (Additive Word) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Word) where
  unit Proxy () = Join 0

instance UnitalMagma (->) (,) (Meet Word) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Word) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Word8) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Word8) where
  unit Proxy () = Join 0

instance UnitalMagma (->) (,) (Meet Word8) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Word8) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Word16) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Word16) where
  unit Proxy () = Join 0

instance UnitalMagma (->) (,) (Meet Word16) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Word16) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Word32) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Word32) where
  unit Proxy () = Join 0

instance UnitalMagma (->) (,) (Meet Word32) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Word32) where
  unit Proxy () = Multiply 1

instance UnitalMagma (->) (,) (Additive Word64) where
  unit Proxy () = Add 0

instance UnitalMagma (->) (,) (Join Word64) where
  unit Proxy () = Join 0

instance UnitalMagma (->) (,) (Meet Word64) where
  unit Proxy () = Meet maxBound

instance UnitalMagma (->) (,) (Multiplicative Word64) where
  unit Proxy () = Multiply 1

instance UnitalMagma (:-) Combine (() :: Constraint) where
  unit Proxy = refl
