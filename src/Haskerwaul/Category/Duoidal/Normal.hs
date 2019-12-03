{-# language UndecidableSuperClasses #-}

module Haskerwaul.Category.Duoidal.Normal
  ( module Haskerwaul.Category.Duoidal.Normal
  -- * extended modules
  , module Haskerwaul.Category.Duoidal
  ) where

import           Data.Functor.Compose (Compose)
import           Data.Functor.Identity (Identity(..))
import           Data.Proxy (Proxy(..))

import Haskerwaul.Category.Duoidal
import Haskerwaul.Day
import Haskerwaul.Bifunctor
import Haskerwaul.Endofunctor
import Haskerwaul.Isomorphism
import Haskerwaul.Subcategory.Full
import Haskerwaul.Transformation.Natural

class DuoidalCategory c di st => NormalDuoidalCategory c di st where
  -- | This must form a lawful isomorphism with `foreUnit`.
  backUnit :: Proxy di -> Proxy st -> Unit c st `c` Unit c di

unitIso :: NormalDuoidalCategory c di st
        => Proxy di -> Proxy st -> Isomorphism c (Unit c di) (Unit c st)
unitIso di st = Iso (foreUnit di st) (backUnit di st)

instance (c ~ (->), ct ~ (,), Bifunctor c c c ct) =>
         NormalDuoidalCategory
         (FullSubcategory (Endofunctor c) (NaturalTransformation (->)))
         (Day c ct ct)
         Compose where
  backUnit Proxy Proxy = FS (NT (\u () -> runIdentity u))
