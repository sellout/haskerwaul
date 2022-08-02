{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Extension.Kan.Right
  ( module Haskerwaul.Extension.Kan.Right,
  )
where

import Data.Constraint ((\\))
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Functor
import Haskerwaul.Object
import Haskerwaul.Semicategory

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/Kan+extension)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Kan_extension)
newtype RightKanExtension c c' p f a
  = Ran (forall b. (Ob c b) => a `c'` p b -> f b)

instance
  (d ~ (->), Semicategory c', FOb (Ob c) (Ob c') p) =>
  Functor c' d (RightKanExtension c c' p f)
  where
  map f (Ran g) =
    Ran (\(h :: b `c'` p x) -> g (h . f) \\ inF @(Ob c) @(Ob c') @p @x)
