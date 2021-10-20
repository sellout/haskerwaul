{-# language TypeApplications
           , UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Duoid
  ( module Haskerwaul.Duoid
  -- * extended modules
  , module Haskerwaul.Duoid.Components
  , module Haskerwaul.Monoid
  ) where

import Haskerwaul.Category.Duoidal
import Haskerwaul.Duoid.Components
import Haskerwaul.Monoid

-- |
-- = laws
--   (see `Haskerwaul.Category.Duoidal.DuoidalCategory`)
--
--   [`interchange`]: @(A ⋆ B) ⋄ (C ⋆ D) → (A ⋄ C) ⋆ (B ⋄ D)@
--   [duplicate unit]: @I → I ⋆ I@
--   [combine unit]: @J ⋄ J → J@
--   [forward unit]: @I →≅ (J ⋆ I) ⋄ (I ⋆ J) → (J ⋄ I) ⋆ (I ⋄ J) →≅ J@ (implied by the other laws)
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/duoidal+category#definition)
--
--  __NB__: Instances for this are automatically coalesced.
class (DuoidalCategory c di st, Monoid c di a, Monoid c st a) =>
      Duoid c di st a

instance (DuoidalCategory c di st, Monoid c di a, Monoid c st a) =>
         Duoid c di st a
