{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Dioid.Commutative
  ( module Haskerwaul.Dioid.Commutative,

    -- * extended modules
    module Haskerwaul.Dioid,
  )
where

import Haskerwaul.Dioid
import Haskerwaul.Lattice.Components (Join (..))
import Numeric.Natural

-- | [nLab](https://ncatlab.org/nlab/show/idempotent+semiring)
--
--   The naming used by nLab here is confusing, as in their lexicon, this should
--   be called an idempotent /rig/. So we choose the (hopefully) less confusing
--   term.
--
-- = laws
--   [commutativity on `multiply`]: @`multiply` x y == `multiply` y x@
class (Dioid c t a) => CommutativeDioid c t a

instance CommutativeDioid (->) (,) (Join Natural)
