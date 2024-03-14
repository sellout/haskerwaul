{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Rig
  ( module Haskerwaul.Category.Rig,

    -- * extended modules
    module Haskerwaul.Category.Rig.ColaxDistributive,
  )
where

import Data.Constraint ((\\))
import Data.Either (Either (..))
import Data.Proxy (Proxy (..))
import qualified Data.Void as Base
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Rig.ColaxDistributive
import Haskerwaul.Functor
import Haskerwaul.Isomorphism
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/rig+category)
class (ColaxDistributiveRigCategory c p s) => RigCategory c p s where
  leftFactor :: (Ob c x, Ob c y, Ob c z) => s (p x y) (p x z) `c` p x (s y z)
  rightFactor :: (Ob c x, Ob c y, Ob c z) => s (p x z) (p y z) `c` p (s x y) z
  leftBeget :: (Ob c a) => Proxy s -> Unit c s `c` p a (Unit c s)
  rightBeget :: (Ob c a) => Proxy s -> Unit c s `c` p (Unit c s) a

leftDistributive ::
  (RigCategory c p s, Ob c x, Ob c y, Ob c z) =>
  Isomorphism c (p x (s y z)) (s (p x y) (p x z))
leftDistributive = Iso leftDistribute leftFactor

rightDistributive ::
  (RigCategory c p s, Ob c x, Ob c y, Ob c z) =>
  Isomorphism c (p (s x y) z) (s (p x z) (p y z))
rightDistributive = Iso rightDistribute rightFactor

leftAnnihilation ::
  (RigCategory c p s, Ob c a) =>
  Proxy s ->
  Isomorphism c (p a (Unit c s)) (Unit c s)
leftAnnihilation p = Iso (leftAnnihilate p) (leftBeget p)

rightAnnihilation ::
  (RigCategory c p s, Ob c a) =>
  Proxy s ->
  Isomorphism c (p (Unit c s) a) (Unit c s)
rightAnnihilation p = Iso (rightAnnihilate p) (rightBeget p)

instance RigCategory (->) (,) Either where
  leftFactor = \case
    Left (a, b) -> (a, Left b)
    Right (a, c) -> (a, Right c)
  rightFactor = \case
    Left (a, c) -> (Left a, c)
    Right (b, c) -> (Right b, c)
  leftBeget Proxy = Base.absurd
  rightBeget Proxy = Base.absurd

-- * additional tensors

-- | Specialized to @`These` (->) (,) `Either` `Void`@, this is like `Either`,
--   and specialized to @`These` (->) (,) `Either` ()@, this is like
--  `Data.These.These`. Other choices for @x@ provide additional variants, and
--   there are corresponding tensors in any `RigCategory`.
--
-- = references
--
-- - [MathOverflow](https://mathoverflow.net/questions/155939/what-other-monoidal-structures-exist-on-the-category-of-sets)
newtype These p s x a b = These (s (p (p a x) b) (s a b))

-- instance (BOb obC obD obC p, BOb obC obD obD s) =>
--          BOb obC obD obD (These p s x) where
--   inB = undefined

instance
  (d ~ (->), Bifunctor c d c p, Bifunctor c d d s, Ob d x) =>
  Bifunctor c d d (These p s x)
  where
  bimap ::
    forall a1 b1 a2 b2.
    (Ob c a1, Ob c b1, Ob d a2, Ob d b2) =>
    a1 `c` b1 ->
    a2 `d` b2 ->
    These p s x a1 a2 `d` These p s x b1 b2
  bimap f g (These t) =
    These (bimap @c @d (bimap (bimap @c @d @c f id) g) (bimap f g) t)
      \\ inB @(Ob c) @(Ob d) @(Ob c) @p @(p a1 x) @a2
      \\ inB @(Ob c) @(Ob d) @(Ob c) @p @(p b1 x) @b2
      \\ inB @(Ob c) @(Ob d) @(Ob c) @p @a1 @x
      \\ inB @(Ob c) @(Ob d) @(Ob c) @p @b1 @x
      \\ inB @(Ob c) @(Ob d) @(Ob d) @s @a1 @a2
      \\ inB @(Ob c) @(Ob d) @(Ob d) @s @b1 @b2

-- instance (c ~ (->), RigCategory c p s) => SemigroupalCategory c (These p s x) where
--   assoc =
--     Iso
--     (\(These r) -> bimap _ (to assoc))
--     (\(These l) -> bimap _ (from assoc))

-- instance RigCategory c p s => MonoidalCategory' c (These p s x) where
--   type Unit c (These p s x) = Unit c s

-- instance RigCategory c p s => MonoidalCategory c (These p s x) where

-- | An alternating sequence of @a@ and @b@, starting and ending with either.
--
-- = references
--
-- - [MathOverflow](https://mathoverflow.net/questions/155939/what-other-monoidal-structures-exist-on-the-category-of-sets)
--
--  __TODO__: Generalize from `[]`, maybe via `Haskerwaul.Sequence`.
newtype Alternating p s x a b
  = Alternating
      ( s
          (p [p (p a x) (p b x)] (s a (p (p a x) b)))
          (p [p (p b x) (p a x)] (s b (p (p b x) a)))
      )

instance
  (c ~ (->), Bifunctor c c c p, Bifunctor c c c s, Ob c x) =>
  Bifunctor c c c (Alternating p s x)
  where
  bimap ::
    forall a1 b1 a2 b2.
    (Ob c a1, Ob c b1, Ob c a2, Ob c b2) =>
    a1 `c` b1 ->
    a2 `c` b2 ->
    Alternating p s x a1 a2 `c` Alternating p s x b1 b2
  bimap f g (Alternating t) =
    Alternating
      ( bimap
          ( bimap @c @c @c
              (map (bimap @c @c @c (bimap @c @c @c f id) (bimap @c @c @c g id)))
              (bimap @c @c @c f (bimap @c @c @c (bimap @c @c @c f id) g))
          )
          ( bimap @c @c @c
              (map (bimap @c @c @c (bimap @c @c @c g id) (bimap @c @c @c f id)))
              (bimap @c @c @c g (bimap @c @c @c (bimap @c @c @c g id) f))
          )
          t
      )

-- instance (c ~ (->), RigCategory c p s) =>
--          SemigroupalCategory c (Alternating p s x) where

-- instance RigCategory c p s => MonoidalCategory' c (Alternating p s x) where
--   type Unit c (Alternating p s x) = Unit c s

-- instance (c ~ (->), RigCategory c p s) =>
--          MonoidalCategory c (Alternating p s x) where

-- -- | This is a tricky type. @unit@ is meant to be the additive unit of a
-- --  `RigCategory` (which is also the unit of this tensor), and the `NoInit` case
-- --   should only be used when neither side matches @unit@.
-- --
-- -- = references
-- --
-- -- - [MathOverflow](https://mathoverflow.net/questions/155939/what-other-monoidal-structures-exist-on-the-category-of-sets)
-- data Both unit a b where
--   LeftInit :: b -> Both unit unit b
--   RightInit :: a -> Both unit a unit
--   NoInit :: Both unit a b

-- type family Both c p s a b where
--   Both c p s (Unit c s) b = b
--   Both c p s a (Unit c s) = a
--   Both c p s a b = Unit c p

-- -- |
-- --  __FIXME__: This instance is incorrect, as a `NoInit`
-- instance RigCategory c p s => Bifunctor c c c (Both c p s) where
--   bimap f g = \case
--     LeftInit b -> LeftInit (g b)
--     RightInit a -> RightInit (f a)
--     NoInit -> NoInit
