{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Rig.ColaxDistributive.Left
  ( module Haskerwaul.Category.Rig.ColaxDistributive.Left,

    -- * extended modules
    module Haskerwaul.Category.Monoidal,
  )
where

import Data.Constraint ((\\))
import Data.Either (Either (..))
import Data.Proxy (Proxy (..))
import qualified Data.Tuple as Base
import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Isomorphism
import Haskerwaul.Object

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/colax-distributive+rig+category)
class
  (MonoidalCategory c p, MonoidalCategory c s) =>
  LeftColaxDistributiveRigCategory c p s
  where
  leftDistribute ::
    (Ob c x, Ob c y, Ob c z) =>
    p x (s y z) `c` s (p x y) (p x z)
  leftAnnihilate :: (Ob c a) => Proxy s -> p a (Unit c s) `c` Unit c s

-- | "If (C,⊗,I) is any monoidal category with finite products, then it becomes
--    a colax-distributive rig category with (C,⊕,J) the cartesian monoidal
--    structure and the distributivity transformations being induced by the
--    universal property of products."
--   -- [nLab](https://ncatlab.org/nlab/show/colax-distributive+rig+category#examples)
--
--   The weakest class we provide with finite products is
--   `CartesianMonoidalCategory`, so we at least provide the instance where the
--   Cartesian product distributes over itself.
instance
  {-# OVERLAPPABLE #-}
  ( CartesianMonoidalCategory c,
    t ~ Prod c,
    -- unnecessary constraints, implied by the ones above
    MonoidalCategory c t
  ) =>
  LeftColaxDistributiveRigCategory c t t
  where
  leftDistribute ::
    forall x y z.
    (Ob c x, Ob c y, Ob c z) =>
    t x (t y z) `c` t (t x y) (t x z)
  leftDistribute =
    from assoc
      . first (Proxy :: Proxy c) (from (assoc . braid @c))
      . to assoc
      . first (Proxy :: Proxy c) (diagonal @_ @c)
      \\ inT @(Ob c) @t @(t x y) @x
      \\ inT @(Ob c) @t @(t x x) @y
      \\ inT @(Ob c) @t @x @y
      \\ inT @(Ob c) @t @y @z
      \\ inT @(Ob c) @t @x @x
  leftAnnihilate Proxy = exr

instance LeftColaxDistributiveRigCategory (->) (,) Either where
  leftDistribute (a, e) = case e of
    Left b -> Left (a, b)
    Right c -> Right (a, c)
  leftAnnihilate Proxy = Base.snd
