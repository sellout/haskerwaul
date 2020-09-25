module Haskerwaul.Category.Rig.ColaxDistributive
  ( module Haskerwaul.Category.Rig.ColaxDistributive
  -- * extended modules
  , module Haskerwaul.Category.Rig.ColaxDistributive.Left
  , module Haskerwaul.Category.Rig.ColaxDistributive.Right
  ) where

import Haskerwaul.Category.Rig.ColaxDistributive.Left
import Haskerwaul.Category.Rig.ColaxDistributive.Right

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/colax-distributive+rig+category)
type ColaxDistributiveRigCategory c p s =
  (LeftColaxDistributiveRigCategory c p s, RightColaxDistributiveRigCategory c p s)
