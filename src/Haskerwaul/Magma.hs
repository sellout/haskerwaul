{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Magma where

import           Data.Constraint ((:-)(..), Dict(..))
import           Data.Either (Either(..))
import qualified Data.Monoid as Base
import qualified Data.Semigroup as Base
import qualified Data.Tuple as Base

import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/magma)
--
--  __NB__: Since this is just any closed binary operation, it's lawless and
--          thus not worth implementing unless there are other instances to
--          implement as well. This exists to complete the diamond where other
--          type classes extend this with various laws and we need them all to
--          apply to the same operation.
class (TOb (Ob c) t, Ob c a) => Magma c t a where
  op :: t a a `c` a

instance {-# overlappable #-} Base.Semigroup a => Magma (->) (,) a where
  op = Base.uncurry (Base.<>)

instance (Magma (->) (,) a, Magma (->) (,) b) => Magma (->) (,) (a, b) where
  op ((x, y), (x', y')) = (op (x, x'), op (y, y'))

instance Magma (->) Either a where
  op = \case
    Left a -> a
    Right a -> a

instance BOb (Magma (->) (,)) (Magma (->) (,)) (Magma (->) (,)) (,) where
  inB = Sub Dict
