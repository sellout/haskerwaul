module Haskerwaul.Monad.Codensity
  ( module Haskerwaul.Monad.Codensity,

    -- * extended modules
    module Haskerwaul.Extension.Kan.Right,
  )
where

import Haskerwaul.Extension.Kan.Right

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/codensity+monad)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Codensity_monad)
type Codensity c d f = RightKanExtension c d f f

-- instance (c ~ (->), CartesianClosedCategory c) => Monad c (Codensity c f) where
--   pure = Ran . to curry (apply . braid)
