{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Quasiorder where

import Data.Proxy (Proxy (..))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Data.Void (Void)
import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Lattice.Complemented.Uniquely
import Haskerwaul.Negation
import Haskerwaul.Object
import Haskerwaul.Order.Total
import Haskerwaul.Topos.Elementary
import qualified Prelude as Base

-- |
--
-- = laws
--
--   [irreflexive]: x≮x
--   [transitive]: If x<y<z, then x<z
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/quasiorder)
class (Ob c a) => Quasiorder c a where
  lt :: BinaryRelation c a a

instance Quasiorder (->) () where
  lt ((), ()) = Base.False

instance Quasiorder (->) Void where
  lt = uncurry $ \case {}

instance
  (c ~ (->), ElementaryTopos c, UniquelyComplementedLattice c (Prod c) (Class c), TotalOrder c a, Ob c (Negate a)) =>
  Quasiorder c (Negate a)
  where
  lt = complement (Proxy @(Prod c)) . le . bimap negation negation . to braid

-- * operators

(<) :: (ElementaryTopos c, Quasiorder c a) => a `c` Exp c a (Class c)
(<) = curry lt

infix 4 <
