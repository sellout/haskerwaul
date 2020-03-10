{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semigroup
  ( module Haskerwaul.Semigroup
  -- * extended modules
  , module Haskerwaul.Magma
  ) where

import           Prelude (Bool, Eq(..))
import           Data.Constraint ((:-)(..), Dict(..))
import           Data.Either (Either)
import qualified Data.Semigroup as Base

import Haskerwaul.Magma
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/semigroup)
--
-- = laws
--   [associative]: op x (op y z) == op (op x y) z
class Magma c t a => Semigroup c t a

associative :: (Semigroup (->) (,) a, Eq a) => a -> a -> a -> Bool
associative x y z = op (x, op (y, z)) == op (op (x, y), z)

-- | Take advantage of this instance when possible. I.e., define your semigroup
--   using `Base.Semigroup` whenever possible.
instance {-# overlappable #-} Base.Semigroup a => Semigroup (->) (,) a

instance (Semigroup (->) (,) a, Semigroup (->) (,) b) => Semigroup (->) (,) (a, b)

instance Semigroup (->) Either a

instance BOb (Semigroup (->) (,)) (Semigroup (->) (,)) (Semigroup (->) (,)) (,) where
  inB = Sub Dict
