{-# language TypeApplications
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Rig.ColaxDistributive.Right
  ( module Haskerwaul.Category.Rig.ColaxDistributive.Right
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Constraint ((\\))
import           Data.Either (Either(..))
import           Data.Proxy (Proxy(..))
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
class (MonoidalCategory c p, MonoidalCategory c s) =>
      RightColaxDistributiveRigCategory c p s where
  rightDistribute :: (Ob c x, Ob c y, Ob c z)
                  => p (s x y) z `c` s (p x z) (p y z)
  rightAnnihilate :: Ob c a => Proxy s -> p (Unit c s) a `c` Unit c s

-- | "If (C,⊗,I) is any monoidal category with finite products, then it becomes
--    a colax-distributive rig category with (C,⊕,J) the cartesian monoidal
--    structure and the distributivity transformations being induced by the
--    universal property of products."
--   -- [nLab](https://ncatlab.org/nlab/show/colax-distributive+rig+category#examples)
--
--   The weakest class we provide with finite products is
--   `CartesianMonoidalCategory`, so we at least provide the instance where the
--   Cartesian product distributes over itself.
instance {-# overlappable #-}
         ( CartesianMonoidalCategory c, t ~ Prod c
         -- unnecessary constraints, implied by the ones above
         , MonoidalCategory c t, MonoidalCategory c t
         ) =>
         RightColaxDistributiveRigCategory c t t where
  rightDistribute :: forall x y z. (Ob c x, Ob c y, Ob c z)
                  => t (t x y) z `c` t (t x z) (t y z)
  rightDistribute =
    to assoc
    . second (Proxy :: Proxy c) (braid @c . to assoc)
    . from assoc
    . second (Proxy :: Proxy c) (diagonal @c)
    \\ inT @(Ob c) @t @z @(t y z)
    \\ inT @(Ob c) @t @y @(t z z)
    \\ inT @(Ob c) @t @x @y
    \\ inT @(Ob c) @t @y @z
    \\ inT @(Ob c) @t @z @z
  rightAnnihilate Proxy = exl

instance RightColaxDistributiveRigCategory (->) (,) (,) where
  rightDistribute ((a, b), c) = ((a, c), (b, c))
  rightAnnihilate Proxy = Base.fst

instance RightColaxDistributiveRigCategory (->) (,) Either where
  rightDistribute (e, c) = case e of
    Left a -> Left (a, c)
    Right b -> Right (b, c)
  rightAnnihilate Proxy = Base.fst
