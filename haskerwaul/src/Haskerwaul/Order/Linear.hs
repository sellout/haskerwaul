{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Order.Linear
  ( module Haskerwaul.Order.Linear,

    -- * extended modules
    module Haskerwaul.Quasiorder,
  )
where

#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Data.Void (Void)
import Haskerwaul.Category.Boolean
import Haskerwaul.Isomorphism
import Haskerwaul.Negation
import Haskerwaul.Order.Total
import Haskerwaul.Quasiorder
import Haskerwaul.Topos.Elementary

-- |
--
-- = laws
--   [asymmetric]: @`lt` (x, y) ==> `Haskerwaul.Lattice.Complemented.Uniquelycomplement` (`lt` (y, x))@
--   [comparison]: @`lt` x z ==> `Haskerwaul.Lattice.join` (`lt` x y) (`lt` y z)@
--   [connectedness]:
--     @`Haskerwaul.Lattice.meet` (`Haskerwaul.Lattice.Complemented.Uniquely.complement` (`lt` x y)) (`Haskerwaul.Lattice.Complemented.Uniquely.complement` (`lt` y x)) ==> `Haskerwaul.Relation.Equality.eq` x y@
--
--   Implies transitive and irreflexive.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/linear+order)
class (Quasiorder c a) => LinearOrder c a

gt ::
  forall c a. (ElementaryTopos c, LinearOrder c a) => HomogeneousRelation c a
gt = lt . to braid

instance LinearOrder (->) ()

instance LinearOrder (->) Void

-- |
--
--  __NB__: @c@ must be a `BooleanCategory` because this definition requires
--          excluded middle.
instance
  (c ~ (->), BooleanCategory c, TotalOrder c a) =>
  LinearOrder c (Negate a)

-- instance LinearOrder c a => HomogeneousRelation c (Negate a) where
--   rel = not . lt . bimap negation negation . braid

-- instance LinearOrder c a => Preorder c (Negate a)

-- instance LinearOrder c a => PartialOrder c (Negate a)

-- -- |
-- --
-- --  __NB__: @c@ must be a `BooleanCategory` because this definition requires
-- --          excluded middle to ensure totality.
-- instance (BooleanCategory c, LinearOrder c a) => TotalOrder c (Negate a)

-- * operators

(>) :: (ElementaryTopos c, LinearOrder c a) => a `c` Exp c a (Class c)
(>) = curry gt
