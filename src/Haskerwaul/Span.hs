module Haskerwaul.Span where

-- | [nLab](https://ncatlab.org/nlab/show/span)
data Span c x y = forall s. Span (s `c` x) (s `c` y)

-- instance Magma (NaturalTransformation2 (->)) CProd (Span c) where
--   op = NT2 (\(CProd n p) -> )

-- instance Semigroup (NaturalTransformation2 (->)) CProd (Span c)

-- instance Semigroupoid c => Semigroupoid (Span c)

-- instance MonoidalCategory' (Span c) t where
--   type Unit (Span c) t = Span c

-- instance UnitalMagma (NaturalTransformation2 (->)) CProd (Span c) where
