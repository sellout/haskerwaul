module Haskerwaul.Automorphism
  ( module Haskerwaul.Automorphism
  -- * extended modules
  , module Haskerwaul.Isomorphism
  ) where

import Haskerwaul.Isomorphism

-- | https://ncatlab.org/nlab/show/automorphism
type Automorphism c a = Isomorphism c a a

-- | https://ncatlab.org/nlab/show/involution
involution :: a `c` a -> Automorphism c a
involution f = Iso f f
