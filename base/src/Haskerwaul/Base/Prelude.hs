{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

-- | This is intended to be a /mostly/ drop-in replacement for "Prelude", but
--   with all the definitions in terms of things from Haskerwaul to improve
--   compatibility.
--
--   This tries to generalize the existing definitions as much as possible to
--   make it easier to generalize your own code that's already defined in terms
--   of base. But we don't recommend using this for new code.
--
--   However, this must(?) expose the original type classes (and thus their
--   methods), which means those are not any more general. But we've tried to
--   ensure that those type classes imply our own, so instances of them should
--   be usable with the rest of "Haskerwaul".
module Haskerwaul.Base.Prelude
  ( module Haskerwaul.Base.Data.Eq,
    module Haskerwaul.Base.Data.Maybe,
    module Haskerwaul.Base.Data.String,
    module Haskerwaul.Base.Text.Show,
    (&&),
    (||),
    not,
    otherwise,
    either,
    fst,
    snd,
    curry,
    uncurry,
    subtract,
  )
where

import Data.Proxy (Proxy (..))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import qualified Haskerwaul as H
import Haskerwaul.Base.Data.Eq (Eq, (/=), (==))
import Haskerwaul.Base.Data.Maybe (Maybe (..))
import Haskerwaul.Base.Data.String (String)
import Haskerwaul.Base.Data.Tuple (curry, uncurry)
import Haskerwaul.Base.Text.Show (Show (..))

-- | Generalization of `Prelude.&&`.
(&&) :: forall c a. (c ~ (->), H.Lattice c (H.Prod c) a) => a `c` H.Exp c a a
(&&) = curry H.meet

infixr 3 &&

-- | Generalization of `Prelude.||`.
(||) :: forall c a. (c ~ (->), H.Lattice c (H.Prod c) a) => a `c` H.Exp c a a
(||) = curry H.join

infixr 2 ||

-- | Generalization of `Prelude.not`.
not :: (H.UniquelyComplementedLattice c (,) a, H.Ob c a) => a `c` a
not = H.complement (Proxy :: Proxy (,))

-- | Generalization of `Prelude.otherwise`.
otherwise :: H.Class (->)
otherwise = H.true ()

-- | Generalization of `Prelude.either`.
either ::
  (H.Bifunctor c1 c2 d f, H.Ob c1 a1, H.Ob c1 b1, H.Ob c2 a2, H.Ob c2 b2) =>
  a1 `c1` b1 ->
  a2 `c2` b2 ->
  f a1 a2 `d` f b1 b2
either = H.bimap

-- | Generalization of `Prelude.fst`.
fst :: (H.CartesianMonoidalCategory c, H.Ob c a, H.Ob c b) => H.Prod c a b `c` a
fst = H.exl

-- | Generalization of `Prelude.snd`.
snd :: (H.CartesianMonoidalCategory c, H.Ob c a, H.Ob c b) => H.Prod c a b `c` b
snd = H.exr

-- | Generalization of `Prelude.subtract`.
subtract ::
  (c ~ (->), H.CartesianClosedMonoidalCategory c, H.Ring c (H.Prod c) a) =>
  a `c` H.Exp c a a
subtract = (H.-)
