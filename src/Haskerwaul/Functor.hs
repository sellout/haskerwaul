{-# language TypeApplications
           , UndecidableInstances #-}

module Haskerwaul.Functor where

import           Data.Constraint ((:-)(..), Dict(..), mapDict)
import qualified Data.Functor as Base
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Const (Const(..))

import Haskerwaul.Object
import Haskerwaul.Semigroupoid

-- | https://ncatlab.org/nlab/show/functor
class FOb (Ob j) (Ob k) f => Functor j k f where
  map :: (Ob j a, Ob j b) => a `j` b -> f a `k` f b

instance (j ~ (->), k ~ (->), Semigroupoid k, Functor j k f, Functor i j g) =>
         Functor i k (Compose f g) where
  map f = Compose . map @j (map @_ @j f) . getCompose

-- | __NB__: This instance only exists to eliminate the ambiguity between the
--          `Base.Functor` constrained instance and the above instance when
--           trying to satisfy @`Functor` (->) (->) (`Compose` f g)@.
instance {-# overlapping #-} (Functor (->) (->) f, Functor (->) (->) g) =>
         Functor (->) (->) (Compose f g) where
  map f = Compose . map @(->) (map @_ @(->) f) . getCompose

instance (j ~ (->), k ~ (->), Semigroupoid k) =>
         BOb (Functor j k) (Functor i j) (Functor i k) Compose where
  inOp = Sub Dict

instance Functor (:-) (->) Dict where
  map = mapDict

instance {-# overlappable #-} Base.Functor f => Functor (->) (->) f where
  map = Base.fmap

-- | The constant functor to a particular object in the target category.
instance (d ~ (->), FOb (Ob c) (Ob d) (Const dOb)) =>
         Functor c d (Const dOb) where
  map _ = Const . getConst
