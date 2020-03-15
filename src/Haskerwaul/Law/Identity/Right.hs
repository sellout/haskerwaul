{-# language TypeApplications #-}

module Haskerwaul.Law.Identity.Right where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Isomorphism
import Haskerwaul.Law

rightIdentityLaw :: forall c t a. (MonoidalCategory c t, UnitalMagma c t a)
                => Law c (t a (Unit c t)) a
rightIdentityLaw =
  Law (to rightIdentity) (op . bimap @c @c id (unit (Proxy :: Proxy t)))
