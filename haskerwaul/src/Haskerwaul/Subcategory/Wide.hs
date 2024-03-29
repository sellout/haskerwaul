{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Haskerwaul.Subcategory.Wide
  ( module Haskerwaul.Subcategory.Wide,

    -- * extended modules
    module Haskerwaul.Subcategory,
  )
where

#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Object
import Haskerwaul.Subcategory

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/wide+subcategory)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Subcategory#Types_of_subcategories)
--
--  __NB__: Instances for this are automatically coalesced.
class (Subcategory c d, Ob c ~ Ob d) => WideSubcategory c d

instance (Subcategory c d, Ob c ~ Ob d) => WideSubcategory c d
