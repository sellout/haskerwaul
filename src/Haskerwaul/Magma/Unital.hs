{-# language UndecidableInstances
           , UndecidableSuperClasses #-}

module Haskerwaul.Magma.Unital
  ( module Haskerwaul.Magma.Unital
  -- * extended modules
  , module Haskerwaul.Category.Monoidal'
  , module Haskerwaul.Magma
  ) where

import           Data.Constraint ((:-)(..), Dict(..))
import           Data.Either (Either)
import qualified Data.Monoid as Base
import           Data.Proxy (Proxy(..))
import qualified Data.Void as Base

import Haskerwaul.Category.Monoidal'
import Haskerwaul.Magma
import Haskerwaul.Object

-- | [nLab](https://ncatlab.org/nlab/show/unital+magma)
--
-- = laws
--   [left identity]: @`op` `unit` x == x@
--   [right identity]: @`op` x `unit` == x@
class (MonoidalCategory' c t, Magma c t a) => UnitalMagma c t a where
  unit :: Proxy t -> Unit c t `c` a

instance {-# overlappable #-} Base.Monoid a => UnitalMagma (->) (,) a where
  unit Proxy () = Base.mempty

instance (UnitalMagma (->) (,) a, UnitalMagma (->) (,) b) =>
         UnitalMagma (->) (,) (a, b) where
  unit t u = (unit t u, unit t u)

instance UnitalMagma (->) Either a where
  unit Proxy = Base.absurd

instance BOb (UnitalMagma (->) (,)) (UnitalMagma (->) (,)) (UnitalMagma (->) (,)) (,) where
  inB = Sub Dict
