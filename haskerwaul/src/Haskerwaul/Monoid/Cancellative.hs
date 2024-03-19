{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Monoid.Cancellative
  ( module Haskerwaul.Monoid.Cancellative,

    -- * extended modules
    module Haskerwaul.Monoid,
  )
where

import Haskerwaul.Monoid

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/cancellative+monoid)
class (Monoid c t a) => CancellativeMonoid c t a
