{-# LANGUAGE Safe #-}

module Haskerwaul.Object.Free where

import Haskerwaul.Category.Closed

-- | https://ncatlab.org/nlab/show/free+object
newtype Free s c a = FreeObject
  { runFree :: forall ob. (s ob) => InternalHom c a ob `c` ob
  }
