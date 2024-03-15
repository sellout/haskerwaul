{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Monad
  ( module Haskerwaul.Monad,

    -- * extended modules
    module Haskerwaul.Monoid,
  )
where

import Data.Functor.Compose (Compose (..))
import Data.Functor.Identity (Identity (..))
import Data.Proxy (Proxy (..))
import Haskerwaul.Endofunctor
import Haskerwaul.Monoid
import Haskerwaul.Object
import Haskerwaul.Semigroupoid
import Haskerwaul.Transformation.Natural

-- | [nLab](https://ncatlab.org/nlab/show/monad)
class (Monoid (NaturalTransformation c c) Compose m, Endofunctor c m) => Monad' c m

instance
  (Monoid (NaturalTransformation c c) Compose m, Endofunctor c m) =>
  Monad' c m

-- | The `Monad` class above is constrained to @(->)@, because it relies on
--  `Compose` and `Identity`. This class is basically a duplicate of it, with
--   the instances of the above class given for free, plus the potential to
--   define more general instances.
class (Endofunctor c m) => Monad c m where
  pure :: (Ob c a) => a `c` m a
  flatten :: (Ob c a) => m (m a) `c` m a

instance {-# OVERLAPPABLE #-} (Monad' (->) m) => Monad (->) m where
  pure = runNT @_ @(->) (unit (Proxy :: Proxy Compose)) . Identity
  flatten = runNT @_ @(->) op . Compose
