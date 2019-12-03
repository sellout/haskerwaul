module Haskerwaul.Pullback where

import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Object.Terminal

-- | https://ncatlab.org/nlab/show/pullback
data Pullback k t a x y =
  Pullback { f :: x `k` a, g :: y `k` a, lx :: t x y `k` x, ly :: t x y `k` y }

type Product k = Pullback k (Prod k) (TerminalObject k)

-- isoSquare
--   :: (SemigroupalCategory k t, Ob k a, Ob k b)
--   => a `k` b -> Iso k a a' -> Iso k b b' -> Pullback k t a b a'
-- isoSquare f gi lxi = Pullback f (to gi) (to lxi . f . from gi) (to lxi)

-- product :: CartesianMonoidalCategoy k => Product k x y
-- product = Pullback Base.absurd Base.absurd exl exr
