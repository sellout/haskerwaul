{-# language UndecidableSuperClasses #-}

module Haskerwaul.Duoid.Normal where

import Haskerwaul.Duoid

-- | [nLab](https://ncatlab.org/nlab/show/duoidal+category#definition)
--
-- = laws
--   [backward unit]: Isomorphism c I J
class Duoid c t a => NormalDuoid c t a
