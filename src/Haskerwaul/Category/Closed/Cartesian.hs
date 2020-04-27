{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Closed.Cartesian
  (module Haskerwaul.Category.Closed.Cartesian
  -- * extended modules
  , module Haskerwaul.Category.Monoidal.Cartesian
  , module Haskerwaul.Category.Monoidal.Closed
  ) where

import qualified Data.Tuple as Base

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Closed
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Isomorphism
import Haskerwaul.Monoid.Commutative.Monus
import Haskerwaul.Object
import Haskerwaul.Rig.Monus
import Haskerwaul.Ring
import Haskerwaul.Skewfield
import Haskerwaul.Subcategory.Full

-- | [nLab](https://ncatlab.org/nlab/show/cartesian+closed+category)
--
--   AKA, "CCC". Operators basically require a CCC, since they tend to be
--   curried versions of morphisms from a product.
class (CartesianMonoidalCategory c, ClosedMonoidalCategory c (Prod c)) =>
      CartesianClosedCategory c where
  -- __TODO__: Is this better as
  --               Isomorphism (->) (Prod c x y `c` z) (x `c` Exp c y z)
  --           or as
  --               Isomorphism c (Exp c (Prod c x y) z) (Exp c x (Exp c y z))
  curry :: (Ob c x, Ob c y, Ob c z)
        => Isomorphism (->) (Prod c x y `c` z) (x `c` Exp c y z)
  -- | this is basically `to curry $ id`, but you need `tuple` in order to
  --   define it that way, so this breaks the cycle.
  tuple :: (Ob c x, Ob c y) => x `c` Exp c y (Prod c x y)

instance CartesianClosedCategory (->) where
  curry = Iso Base.curry Base.uncurry
  tuple = (,)

instance ( CartesianClosedCategory c
         , TOb ob (Prod c), ob (TerminalObject c), TOb ob (Exp c)) =>
         CartesianClosedCategory (FullSubcategory ob c) where
  curry = Iso (FS . to curry . inclusion) (FS . from curry . inclusion)
  tuple = FS tuple

-- * Operators
--
-- In order to have binary operators, we must be able to uncurry binary
-- operations. This requires at least a CCC. Ideally, these operators would work
-- for _any_ CCC, but currently they require __Set__ specifically. Hopefully we
-- can eventually remove the `~` constraint.

($) :: (c ~ (->), CartesianClosedCategory c, Ob c a, Ob c b)
    => (a `c` b) `c` Exp c a b
($) = to curry apply

(<>) :: (c ~ (->), CartesianClosedCategory c, Magma c (Prod c) a) => a `c` Exp c a a
(<>) = to curry op

(.-) :: (c ~ (->), CartesianClosedCategory c, RigMonus c (Prod c) a) => a `c` Exp c a a
(.-) = to curry (sum . monus . bimap Add Add)

(+) :: (c ~ (->), CartesianClosedCategory c, NearPreSemiring c (Prod c) a)
    => a `c` Exp c a a
(+) = to curry add

(*) :: (c ~ (->), CartesianClosedCategory c, NearPreSemiring c (Prod c) a)
    => a `c` Exp c a a
(*) = to curry multiply

(-) :: (c ~ (->), CartesianClosedCategory c, Ring c (Prod c) a) => a `c` Exp c a a
(-) = to curry subtract

(/) :: (c ~ (->), CartesianClosedCategory c, Skewfield c (Prod c) a) => a `c` Exp c a a
(/) = to curry divide
