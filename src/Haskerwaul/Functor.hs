{-# language TypeApplications
           , UndecidableInstances #-}

module Haskerwaul.Functor where

import           Data.Constraint ((:-)(..), Dict(..), mapDict)
import qualified Data.Functor as Base
import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Const (Const(..))

import Haskerwaul.Object
import Haskerwaul.Semigroupoid

-- | [nLab](https://ncatlab.org/nlab/show/functor)
class FOb (Ob c) (Ob d) f => Functor c d f where
  map :: (Ob c a, Ob c b) => a `c` b -> f a `d` f b

instance (b ~ (->), c ~ (->), Semigroupoid c, Functor b c f, Functor a b g) =>
         Functor a c (Compose f g) where
  map f = Compose . map @b (map @_ @b f) . getCompose

-- | __NB__: This instance only exists to eliminate the ambiguity between the
--          `Base.Functor` constrained instance and the above instance when
--           trying to satisfy @`Functor` (->) (->) (`Compose` f g)@.
instance {-# overlapping #-} (Functor (->) (->) f, Functor (->) (->) g) =>
         Functor (->) (->) (Compose f g) where
  map f = Compose . map @(->) (map @_ @(->) f) . getCompose

instance (b ~ (->), c ~ (->), Semigroupoid c) =>
         BOb (Functor b c) (Functor a b) (Functor a c) Compose where
  inB = Sub Dict

instance Functor (:-) (->) Dict where
  map = mapDict

instance {-# overlappable #-} Base.Functor f => Functor (->) (->) f where
  map = Base.fmap

-- | The constant functor to a particular object in the target category.
instance (d ~ (->), FOb (Ob c) (Ob d) (Const dOb)) =>
         Functor c d (Const dOb) where
  map _ = Const . getConst
