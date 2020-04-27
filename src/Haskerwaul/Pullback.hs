module Haskerwaul.Pullback where

import Haskerwaul.Category.Monoidal.Cartesian

-- | [nLab](https://ncatlab.org/nlab/show/pullback)
data Pullback c t a x y =
  Pullback { f :: x `c` a, g :: y `c` a, lx :: t x y `c` x, ly :: t x y `c` y }

type Product c = Pullback c (Prod c) (TerminalObject c)

-- isoSquare
--   :: (SemigroupalCategory c t, Ob c a, Ob c b)
--   => a `c` b -> Iso c a a' -> Iso c b b' -> Pullback c t a b a'
-- isoSquare f gi lxi = Pullback f (to gi) (to lxi . f . from gi) (to lxi)

-- product :: CartesianMonoidalCategoy c => Product c x y
-- product = Pullback Base.absurd Base.absurd exl exr
