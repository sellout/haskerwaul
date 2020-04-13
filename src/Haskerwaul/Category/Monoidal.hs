{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal
  ( module Haskerwaul.Category.Monoidal
  -- * extended modules
  , module Haskerwaul.Category.Monoidal'
  , module Haskerwaul.Category.Semigroupal
  ) where

import qualified Control.Category as Base
import           Data.Constraint ((:-), (&&&), cls, ins, refl, trans, top, weaken1, weaken2)
import           Data.Either (Either(..))
import qualified Data.Either as Base
import           Data.Functor.Const (Const(..))
import           Data.Proxy (Proxy(..))
import qualified Data.Tuple as Base
import qualified Data.Void as Base

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Opposite
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Constraint
import Haskerwaul.Bifunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/monoidal+category)
class (SemigroupalCategory c t, MonoidalCategory' c t) =>
      MonoidalCategory c t where
  leftIdentity :: Ob c a => Isomorphism c (t (Unit c t) a) a
  rightIdentity :: Ob c a => Isomorphism c (t a (Unit c t)) a

-- | Every monoidal structure is monoidal in the opposite category.
instance MonoidalCategory c t => MonoidalCategory (Opposite c) t where
  leftIdentity = runNT2 (to isomorphismOp) leftIdentity
  rightIdentity = runNT2 (to isomorphismOp) rightIdentity

instance MonoidalCategory (->) (,) where
  leftIdentity = Iso Base.snd ((), )
  rightIdentity = Iso Base.fst (, ())

instance MonoidalCategory (->) Either where
  leftIdentity = Iso (Base.either Base.absurd Base.id) Right
  rightIdentity = Iso (Base.either Base.id Base.absurd) Left

instance (c ~ (->), MonoidalCategory c t) =>
         MonoidalCategory (NaturalTransformation c) (FTensor t) where
  leftIdentity =
    Iso
    (NT (to leftIdentity . first p getConst . lowerFTensor))
    (NT (FTensor . first p Const . from leftIdentity))
   where
    p :: Proxy c
    p = Proxy
  rightIdentity =
    Iso
    (NT (to rightIdentity . second p getConst . lowerFTensor))
    (NT (FTensor . second p Const . from rightIdentity))
   where
    p :: Proxy c
    p = Proxy

instance (c ~ (->), MonoidalCategory c t) =>
         MonoidalCategory (NaturalTransformation2 c) (BTensor t) where
  leftIdentity =
    Iso
    (NT2 (to leftIdentity . first p getBConst . lowerBTensor))
    (NT2 (BTensor . first p BConst . from leftIdentity))
   where
    p :: Proxy c
    p = Proxy
  rightIdentity =
    Iso
    (NT2 (to rightIdentity . second p getBConst . lowerBTensor))
    (NT2 (BTensor . second p BConst . from rightIdentity))
   where
    p :: Proxy c
    p = Proxy

instance MonoidalCategory (:-) Combine where
  leftIdentity = Iso (trans weaken2 cls) (trans ins (top &&& refl))
  rightIdentity = Iso (trans weaken1 cls) (trans ins (refl &&& top))

instance MonoidalCategory (NaturalTransformation (:-)) CFProd where
  leftIdentity =
    Iso
    (NT (trans weaken2 cls))
    (NT (trans ins (trans ins top &&& refl)))
  rightIdentity =
    Iso
    (NT (trans weaken1 cls))
    (NT (trans ins (refl &&& trans ins top)))
