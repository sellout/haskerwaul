{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Dioid
  ( module Haskerwaul.Dioid,

    -- * extended modules
    module Haskerwaul.Rig,
  )
where

import Haskerwaul.Lattice.Components (Join (..))
import Haskerwaul.Rig
import Numeric.Natural

-- | [nLab](https://ncatlab.org/nlab/show/idempotent+semiring)
--
--   The naming used by nLab here is confusing, as in their lexicon, this should
--   be called an idempotent /rig/. So we choose the (hopefully) less confusing
--   term.
class (Rig c t a) => Dioid c t a

instance Dioid (->) (,) (Join Natural)
