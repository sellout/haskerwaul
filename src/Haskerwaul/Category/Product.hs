module Haskerwaul.Category.Product where

-- | [nLab](https://ncatlab.org/nlab/show/product+category)
data ProductCategory c d a b = ProdC (a `c` b) (a `d` b)
