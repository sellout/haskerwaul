{-# language UndecidableSuperClasses #-}

module Haskerwaul.Dioid where

import Haskerwaul.Rig

-- | https://ncatlab.org/nlab/show/idempotent+semiring
--   The naming used by nLab here is confusing, as in their lexicon, this should
--   be called an idempotent /rig/. So we choose the (hopefully) less confusing
--   term.
class Rig c t a => Dioid c t a
