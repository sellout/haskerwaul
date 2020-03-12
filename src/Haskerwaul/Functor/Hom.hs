module Haskerwaul.Functor.Hom
  ( module Haskerwaul.Functor.Hom
  -- * extended modules
  , module Haskerwaul.Profunctor
  ) where

import Haskerwaul.Profunctor

type HomFunctor c = Profunctor c c
