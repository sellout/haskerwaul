module Haskerwaul.Pullback where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Object

-- |
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/pullback)
-- - [Wikipedia](https://en.wikipedia.org/wiki/Pullback_(category_theory))
data PullbackSquare c t x y = forall a. PullbackSquare
  { lx :: t x y `c` x,
    ly :: t x y `c` y,
    f :: x `c` a,
    g :: y `c` a
  }

-- inPullback :: (t x y, PullbackSquare c t x y) `c` Class c
-- inPullback (prod, pb) =
--   uncurry (==) . (f pb . lx pb &&& g pb . ly pb) $ prod

cartesianPullbackSquare
  :: (CartesianMonoidalCategory c, Ob c x, Ob c y)
  => x `c` a -> y `c` a -> PullbackSquare c (Prod c) x y
cartesianPullbackSquare = PullbackSquare exl exr

type Product c = PullbackSquare c (Prod c) (TerminalObject c)

-- isoSquare
--   :: (SemigroupalCategory c t, Ob c a, Ob c b)
--   => a `c` b -> Iso c a a' -> Iso c b b' -> Pullback c t a b a'
-- isoSquare f gi lxi = Pullback f (to gi) (to lxi . f . from gi) (to lxi)

-- product :: CartesianMonoidalCategoy c => Product c x y
-- product = Pullback Base.absurd Base.absurd exl exr
