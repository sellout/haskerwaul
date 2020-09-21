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
module Haskerwaul.Base.Prelude where

import           Data.Proxy (Proxy (..))

import qualified Haskerwaul as H

-- | Generalization of `Prelude.&&`.
(&&) :: (c ~ (->), H.CartesianClosedCategory c, H.Lattice c (H.Prod c) a) => a `c` H.Exp c a a
(&&) = H.to H.curry H.meet

-- | Generalization of `Prelude.||`.
(||) :: (c ~ (->), H.CartesianClosedCategory c, H.Lattice c (H.Prod c) a) => a `c` H.Exp c a a
(||) = H.to H.curry H.join

-- | Generalization of `Prelude.not`.
not :: (H.UniquelyComplementedLattice c (,) a, H.Ob c a) => a `c` a
not = H.complement (Proxy :: Proxy (,))

-- | Generalization of `Prelude.otherwise`.
otherwise :: H.Class (->)
otherwise = H.true ()

-- | Generalization of `Prelude.either`.
either :: (H.Bifunctor c1 c2 d f, H.Ob c1 a1, H.Ob c1 b1, H.Ob c2 a2, H.Ob c2 b2)
       => a1 `c1` b1 -> a2 `c2` b2 -> f a1 a2 `d` f b1 b2
either = H.bimap

-- | Generalization of `Prelude.fst`.
fst :: (H.CartesianMonoidalCategory c, H.Ob c a, H.Ob c b) => H.Prod c a b `c` a
fst = H.exl

-- | Generalization of `Prelude.snd`.
snd :: (H.CartesianMonoidalCategory c, H.Ob c a, H.Ob c b) => H.Prod c a b `c` b
snd = H.exr

-- | Generalization of `Prelude.curry`.
curry :: (H.CartesianClosedCategory c, H.Ob c x, H.Ob c y, H.Ob c z)
      => H.Prod c x y `c` z -> x `c` H.Exp c y z
curry = H.to H.curry

-- | Generalization of `Prelude.uncurry`.
uncurry :: (H.CartesianClosedCategory c, H.Ob c x, H.Ob c y, H.Ob c z)
        => x `c` H.Exp c y z -> H.Prod c x y `c` z
uncurry = H.from H.curry

-- | Generalization of `Prelude.subtract`.
subtract :: (c ~ (->), H.CartesianClosedCategory c, H.Ring c (H.Prod c) a) => a `c` H.Exp c a a
subtract = H.to H.curry H.subtract
