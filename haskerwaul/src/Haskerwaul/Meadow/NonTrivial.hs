{-# language UndecidableSuperClasses #-}

module Haskerwaul.Meadow.NonTrivial
  ( module Haskerwaul.Meadow.NonTrivial
  -- * extended modules
  , module Haskerwaul.Meadow
  ) where

-- import Data.Proxy (Proxy(..))

import Haskerwaul.Meadow
-- import Haskerwaul.Topos.Elementary

-- | [Division by zero in non-involutive meadows](https://www.sciencedirect.com/science/article/pii/S1570868314000652)
--
-- = laws
--   [separation]: 0 /= 1
class Meadow c t a => NonTrivialMeadow c t a

-- -- | In a lawful `NonTrivialMeadow`, given an equivalence relation, the resulting `Class c` should alway be equivalent to `true`.
-- separation :: (ElementaryTopos c, NonTrivialMeadow c t a) => Exp c (t a a) (Class c) `c` Class c
-- separation = apply . braid . tuple (fork (sum . unit Proxy) (product . unit Proxy))

