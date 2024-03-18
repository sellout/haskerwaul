{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Relation.Tolerance
  ( module Haskerwaul.Relation.Tolerance,

    -- * extended modules
    module Haskerwaul.Relation.Homogeneous,
  )
where

#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Data.Void (Void)
import Haskerwaul.Lattice.Components
import Haskerwaul.Object
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Semiring.Components
import Haskerwaul.Topos.Elementary

-- | [Wikipedia](https://en.wikipedia.org/wiki/Tolerance_relation)
--
-- = laws
--
-- - [`Haskerwaul.Law.Reflexivity.reflexivity`]: @`rel` x x = `true`@
-- - [`Haskerwaul.Law.Symmetry.symmetry`]: @`rel` x y ==> `rel` y x@
class (HomogeneousRelation' c a) => ToleranceRelation c a

instance ToleranceRelation (->) ()

instance ToleranceRelation (->) Void

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Meet a)) =>
  ToleranceRelation c (Meet a)

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Join a)) =>
  ToleranceRelation c (Join a)

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Additive a)) =>
  ToleranceRelation c (Additive a)

instance
  {-# INCOHERENT #-}
  (c ~ (->), ElementaryTopos c, ToleranceRelation c a, Ob c (Multiplicative a)) =>
  ToleranceRelation c (Multiplicative a)
