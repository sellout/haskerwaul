module Haskerwaul.Cone where

-- | [nLab](https://ncatlab.org/nlab/show/cone)
data Cone k d c = Cone (c `k` d)
