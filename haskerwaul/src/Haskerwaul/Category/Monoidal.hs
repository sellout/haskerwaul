{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal
  ( module Haskerwaul.Category.Monoidal,

    -- * extended modules
    module Haskerwaul.Category.MonoidalUnit,
    module Haskerwaul.Category.Semigroupal,
  )
where

import qualified Control.Category as Base
import Data.Constraint (Dict (Dict), (:-) (Sub), type (&))
import Data.Either (Either (..))
import qualified Data.Either as Base
import Data.Functor.Const (Const (..))
import Data.Proxy (Proxy (..))
import qualified Data.Tuple as Base
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality ((:~:) (..), type (~))
#else
import Data.Type.Equality ((:~:) (..))
#endif
import qualified Data.Void as Base
import Haskerwaul.Bifunctor
import Haskerwaul.Category.MonoidalUnit
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Constraint
import Haskerwaul.Isomorphism
import Haskerwaul.Object
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/monoidal+category)
class
  (SemigroupalCategory c t, MonoidalCategory' c t) =>
  MonoidalCategory c t
  where
  leftIdentity :: (Ob c a) => Isomorphism c (t (Unit c t) a) a
  rightIdentity :: (Ob c a) => Isomorphism c (t a (Unit c t)) a

instance MonoidalCategory (->) (,) where
  leftIdentity = Iso Base.snd ((),)
  rightIdentity = Iso Base.fst (,())

instance MonoidalCategory (->) Either where
  leftIdentity = Iso (Base.either Base.absurd Base.id) Right
  rightIdentity = Iso (Base.either Base.id Base.absurd) Left

instance
  (d ~ (->), MonoidalCategory d dt) =>
  MonoidalCategory (NaturalTransformation c d) (FTensor dt)
  where
  leftIdentity =
    Iso
      (NT (to leftIdentity . first p getConst . lowerFTensor))
      (NT (FTensor . first p Const . from leftIdentity))
    where
      p :: Proxy d
      p = Proxy
  rightIdentity =
    Iso
      (NT (to rightIdentity . second p getConst . lowerFTensor))
      (NT (FTensor . second p Const . from rightIdentity))
    where
      p :: Proxy d
      p = Proxy

instance MonoidalCategory (DinaturalTransformation (->)) Procompose where
  leftIdentity = Iso (DT (\(Procompose Refl a) -> a)) (DT (Procompose Refl))
  rightIdentity = Iso (DT (\(Procompose a Refl) -> a)) (DT (`Procompose` Refl))

instance
  (c ~ (->), MonoidalCategory c t) =>
  MonoidalCategory (DinaturalTransformation c) (BTensor t)
  where
  leftIdentity =
    Iso
      (DT (to leftIdentity . first p getBConst . lowerBTensor))
      (DT (BTensor . first p BConst . from leftIdentity))
    where
      p :: Proxy c
      p = Proxy
  rightIdentity =
    Iso
      (DT (to rightIdentity . second p getBConst . lowerBTensor))
      (DT (BTensor . second p BConst . from rightIdentity))
    where
      p :: Proxy c
      p = Proxy

instance (MonoidalCategory c t) => MonoidalCategory (Isomorphism c) t where
  leftIdentity = Iso leftIdentity (reverse leftIdentity)
  rightIdentity = Iso rightIdentity (reverse rightIdentity)

instance MonoidalCategory (:-) (&) where
  leftIdentity = Iso (Sub Dict) (Sub Dict)
  rightIdentity = Iso (Sub Dict) (Sub Dict)

instance MonoidalCategory (NaturalTransformation c (:-)) CFProd where
  leftIdentity = Iso (NT (Sub Dict)) (NT (Sub Dict))
  rightIdentity = Iso (NT (Sub Dict)) (NT (Sub Dict))
