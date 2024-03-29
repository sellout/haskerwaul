{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Ring
  ( module Haskerwaul.Ring,

    -- * extended modules
    module Haskerwaul.Group.Abelian,
    module Haskerwaul.Rig,
    module Haskerwaul.Ring.Nonunital,
  )
where

#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Group
import Haskerwaul.Group.Abelian
import Haskerwaul.Rig
import Haskerwaul.Ring.Nonunital

-- | [nLab](https://ncatlab.org/nlab/show/ring)
--
--  __NB__: Instances for this are automatically coalesced.
class
  (AbelianGroup c t (Additive a), Rig c t a, NonunitalRing c t a) =>
  Ring c t a

instance
  (AbelianGroup c t (Additive a), Monoid c t (Multiplicative a)) =>
  Ring c t a

subtract ::
  (c ~ (->), SemigroupalCategory c t, Ring c t a) =>
  t a a `c` a
subtract = sum . quotient . bimap Add Add

negate :: (c ~ (->), CartesianMonoidalCategory c, Ring c (Prod c) a) => a `c` a
negate = sum . inverse . Add
