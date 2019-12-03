{-# language UndecidableSuperClasses #-}

module Haskerwaul.Semiring.Idempotent where

import Haskerwaul.Rig

-- | https://ncatlab.org/nlab/show/idempotent+semiring
--   Also known as a "dioid".
class Rig c t a => IdempotentSemiring c t a
