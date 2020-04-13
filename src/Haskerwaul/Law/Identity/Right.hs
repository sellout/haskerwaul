{-# language TypeApplications #-}

module Haskerwaul.Law.Identity.Right where

import Haskerwaul.Bifunctor
import Haskerwaul.Category.Monoidal
import Haskerwaul.Isomorphism
import Haskerwaul.Law
import Haskerwaul.Object

rightIdentityLaw :: forall c t a. (MonoidalCategory c t, Ob c a)
                => t a a `c` a -> Unit c t `c` a -> Law c (t a (Unit c t)) a
rightIdentityLaw op' unit' = Law (to rightIdentity) (op' . bimap @c id unit')
