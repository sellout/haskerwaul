{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Closed.Cartesian
  ( module Haskerwaul.Category.Monoidal.Closed.Cartesian,

    -- * extended modules
    module Haskerwaul.Category.Closed.Cartesian,
    module Haskerwaul.Category.Monoidal.Cartesian,
    module Haskerwaul.Category.Monoidal.Closed,
    module Haskerwaul.Object.Exponential,
  )
where

import Data.Constraint ((\\))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Isomorphism
import Haskerwaul.Monoid.Commutative.Monus
import Haskerwaul.Object
import Haskerwaul.Object.Exponential
import Haskerwaul.Rig.Monus
import Haskerwaul.Ring
import Haskerwaul.Skewfield

-- | Also known as a "CCC". __NB__: There is a /distinct/ concept of a
--  `CartesianClosedCategory` which is basically this without the monoidal
--   structure. This is /probably/ the one you want, but the naming is uncommon
--   and meant for disambiguation.
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/cartesian+closed+category)
class
  (CartesianMonoidalCategory c, CartesianClosedCategory c, ClosedMonoidalCategory c (Prod c)) =>
  CartesianClosedMonoidalCategory c
  where
  -- | this is basically @`to` `curry` `$` `id`@, but you need `tuple` in order
  --   to define it that way, so this breaks the cycle.
  tuple :: (Ob c x, Ob c y) => x `c` Exp c y (Prod c x y)

curryIso ::
  (CartesianClosedMonoidalCategory c, Ob c x, Ob c y, Ob c z) =>
  Isomorphism (->) (Prod c x y `c` z) (x `c` Exp c y z)
curryIso = Iso curry uncurry

instance CartesianClosedMonoidalCategory (->) where
  tuple = (,)

-- * Operators

--   In order to have binary /operators/, we must be able to `curry` binary
--  /operations/. This requires at least a closed monoidal category. However, we
--   also can't afford to deal with a Proxy (or type application) to
--   monomorphize the tensor, so we require a `CartesianClosedMonoidalCategory`,
--   since that selects a particular tensor for us.

($) ::
  forall c a b.
  (CartesianClosedMonoidalCategory c, Ob c a, Ob c b) =>
  InternalHom c a b `c` InternalHom c a b
($) = curry @_ @(Prod c) apply \\ inT @(Ob c) @(InternalHom c) @a @b

infix 0 $

(<>) ::
  forall c a.
  (CartesianClosedMonoidalCategory c, Magma c (Prod c) a) =>
  a `c` InternalHom c a a
(<>) = curry @_ @(Prod c) op

infixr 6 <>

(.-) ::
  forall c a.
  (c ~ (->), CartesianClosedMonoidalCategory c, RigMonus c (Prod c) a) =>
  a `c` InternalHom c a a
(.-) = curry @_ @(Prod c) (sum . monus . bimap Add Add)

infixl 6 .-

(+) ::
  forall c a.
  (c ~ (->), CartesianClosedMonoidalCategory c, NearPreSemiring c (Prod c) a) =>
  a `c` InternalHom c a a
(+) = curry @_ @(Prod c) add

infixl 6 +

(*) ::
  forall c a.
  (c ~ (->), CartesianClosedMonoidalCategory c, NearPreSemiring c (Prod c) a) =>
  a `c` InternalHom c a a
(*) = curry @_ @(Prod c) multiply

infixl 7 *

(-) ::
  forall c a.
  (c ~ (->), CartesianClosedMonoidalCategory c, Ring c (Prod c) a) =>
  a `c` InternalHom c a a
(-) = curry @_ @(Prod c) subtract

infixl 6 -

(/) ::
  forall c a.
  (c ~ (->), CartesianClosedMonoidalCategory c, Skewfield c (Prod c) a) =>
  a `c` InternalHom c a a
(/) = curry @_ @(Prod c) divide

infixl 7 /
