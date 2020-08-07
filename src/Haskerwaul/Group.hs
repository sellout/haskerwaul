{-# language UndecidableSuperClasses #-}

module Haskerwaul.Group
  ( module Haskerwaul.Group
  -- * extended modules
  , module Haskerwaul.Loop
  , module Haskerwaul.Monoid
  ) where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Loop
import Haskerwaul.Monoid

-- | [nLab](https://ncatlab.org/nlab/show/group)
--
--  __NB__: Instances for this are automatically coalesced.
class (Loop c t a, Monoid c t a) => Group c t a

instance (Loop c t a, Monoid c t a) => Group c t a

inverse :: (CartesianMonoidalCategory c, Group c (Prod c) a) => a `c` a
inverse = rightInverse

quotient :: Group c t a => t a a `c` a
quotient = rightQuotient

-- * `Haskerwaul.Groupoid.Groupoid` instances

-- instance Group (DinaturalTransformation (->)) Procompose (:~:) where
--   inverse = DT sym

-- instance Group (DinaturalTransformation (->)) Procompose (Iso c) where
--   inverse = DT (\(Iso to from) -> Iso from to)
