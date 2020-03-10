{-# language UndecidableSuperClasses #-}

module Haskerwaul.Group
  ( module Haskerwaul.Group
  -- * extended modules
  , module Haskerwaul.Loop
  , module Haskerwaul.Monoid
  ) where

import           Data.Proxy (Proxy)

import Haskerwaul.Loop
import Haskerwaul.Monoid

-- | [nLab](https://ncatlab.org/nlab/show/group)
class (Loop c t a, Monoid c t a) => Group c t a

instance (Loop c t a, Monoid c t a) => Group c t a

inverse :: (c ~ (->), t ~ (,), Group c t a) => Proxy t -> a `c` a
inverse = rightInverse

quotient :: Group c t a => t a a `c` a
quotient = rightQuotient

-- * `Haskerwaul.Groupoid.Groupoid` instances

-- instance Group (NaturalTransformation2 (->)) CProd (:~:) where
--   inverse = NT2 sym

-- instance Group (NaturalTransformation2 (->)) CProd (Iso c) where
--   inverse = NT2 (\(Iso to from) -> Iso from to)
