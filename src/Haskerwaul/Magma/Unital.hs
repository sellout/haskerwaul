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

-- | https://ncatlab.org/nlab/show/unital+magma
class (MonoidalCategory' k t, Magma k t a) => UnitalMagma k t a where
  unit :: Proxy t -> Unit k t `k` a

-- leftIdentity :: (UnitalMagma (->) (,) a, Eq a) => a -> Bool
-- leftIdentity x = op (unit (), x) == x

-- rightIdentity :: (UnitalMagma (->) (,) a, Eq a) => a -> Bool
-- rightIdentity x = op (x, unit ()) == x

instance {-# overlappable #-} Base.Monoid a => UnitalMagma (->) (,) a where
  unit Proxy () = Base.mempty

instance (UnitalMagma (->) (,) a, UnitalMagma (->) (,) b) =>
         UnitalMagma (->) (,) (a, b) where
  unit t u = (unit t u, unit t u)

instance UnitalMagma (->) Either a where
  unit Proxy = Base.absurd

instance BOb (UnitalMagma (->) (,)) (UnitalMagma (->) (,)) (UnitalMagma (->) (,)) (,) where
  inOp = Sub Dict
