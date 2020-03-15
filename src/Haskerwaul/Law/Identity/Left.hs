{-# language TypeApplications #-}

module Haskerwaul.Law.Identity.Left where

import           Data.Proxy (Proxy(..))

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Isomorphism
import Haskerwaul.Law

leftIdentityLaw :: forall c t a. (MonoidalCategory c t, UnitalMagma c t a)
                => Law c (t (Unit c t) a) a
leftIdentityLaw =
  Law (to leftIdentity) (op . bimap @c @c (unit (Proxy :: Proxy t)) id)
