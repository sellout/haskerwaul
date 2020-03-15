module Haskerwaul.Object.Free where

import Haskerwaul.Category.Closed

-- | https://ncatlab.org/nlab/show/free+object
data Free s c a = FreeObject { runFree :: forall ob. s ob => (Exp c a ob) `c` ob }
