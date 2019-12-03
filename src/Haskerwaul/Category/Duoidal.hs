{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Category.Duoidal
  ( module Haskerwaul.Category.Duoidal
  -- * extended modules
  , module Haskerwaul.Category.Monoidal
  ) where

import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Day
import Haskerwaul.Endofunctor
import Haskerwaul.Object
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

-- | https://ncatlab.org/nlab/show/duoidal+category
class (MonoidalCategory k di, MonoidalCategory k st) =>
      DuoidalCategory k di st where
  switch :: (Ob k a, Ob k b, Ob k c, Ob k d)
         => di (st a b) (st c d) `k` st (di a c) (di b d)
  duplicateUnit :: Proxy di -> Unit k di `k` st (Unit k di) (Unit k di)
  combineUnit :: Proxy st -> di (Unit k st) (Unit k st) `k` Unit k st
  foreUnit :: Proxy di -> Proxy st -> Unit k di `k` Unit k st

-- | This is currently specialized to `Endofunctor`s over *Set*. Should be able to generalize this.
instance (c ~ (->), ct ~ (,), Bifunctor c c c ct) =>
         DuoidalCategory
         (FullSubcategory (Endofunctor c) (NaturalTransformation (->)))
         (Day c ct ct)
         Compose where
  switch =
    FS (NT (\(Day t fn) -> Compose (Day (bimap getCompose getCompose t) (\t' -> Day t' fn))))
  duplicateUnit Proxy = FS (NT (\const -> Compose (\_ -> const)))
  combineUnit Proxy =
    FS (NT (\(Day t fn) -> Identity (fn (bimap runIdentity runIdentity t))))
  foreUnit Proxy Proxy = FS (NT (\const -> Identity (const ())))
