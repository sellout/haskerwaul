{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Category.Monoidal.Cartesian
  ( module Haskerwaul.Category.Monoidal.Cartesian,

    -- * extended modules
    module Haskerwaul.Category.Monoidal.Symmetric,
    module Haskerwaul.Object.Terminal,
  )
where

import Data.Constraint (Dict (Dict), (:-) (Sub), type (&))
import Data.Kind (Type)
import qualified Data.Tuple as Base
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Object.Terminal
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- |
--
-- = references
--
-- - [nLab](https://ncatlab.org/nlab/show/cartesian+monoidal+category)
class
  ( SymmetricMonoidalCategory c (Prod c),
    HasTerminalObject c,
    Unit c (Prod c) ~ TerminalObject c
  ) =>
  CartesianMonoidalCategory (c :: ok -> ok -> Type)
  where
  -- |
  --
  -- = references
  --
  -- - [nLab](https://ncatlab.org/nlab/show/cartesian+product)
  type Prod c :: ok -> ok -> ok

  -- |
  --
  -- = references
  --
  -- - [nLab](https://ncatlab.org/nlab/show/projection)
  exl :: (Ob c a, Ob c b) => Prod c a b `c` a

  -- |
  --
  -- = references
  --
  -- - [nLab](https://ncatlab.org/nlab/show/projection)
  exr :: (Ob c a, Ob c b) => Prod c a b `c` b

  -- |
  --
  -- = references
  --
  -- - [nLab](https://ncatlab.org/nlab/show/diagonal+morphism)
  diagonal :: (Ob c a) => a `c` Prod c a a

instance CartesianMonoidalCategory (->) where
  type Prod (->) = (,)
  exl = Base.fst
  exr = Base.snd
  diagonal x = (x, x)

-- |
--
--  __TODO__: Generalizing this to @(d `~` (->), `CartesianMonoidalCategory` d) =>@
--            leads to conflicting family instances with the
--            @`NaturalTransformation` c (`:-`)@ instance below.
instance CartesianMonoidalCategory (NaturalTransformation c (->)) where
  type Prod (NaturalTransformation c (->)) = FTensor (,)
  exl = NT (exl . lowerFTensor)
  exr = NT (exr . lowerFTensor)
  diagonal = NT (FTensor . diagonal)

instance CartesianMonoidalCategory (:-) where
  type Prod (:-) = (&)
  exl = Sub Dict
  exr = Sub Dict
  diagonal = Sub Dict

instance CartesianMonoidalCategory (NaturalTransformation c (:-)) where
  type Prod (NaturalTransformation c (:-)) = CFProd
  exl = NT (Sub Dict)
  exr = NT (Sub Dict)
  diagonal = NT (Sub Dict)

instance
  (d ~ (->), CartesianMonoidalCategory d) =>
  CartesianMonoidalCategory (DinaturalTransformation d)
  where
  type Prod (DinaturalTransformation d) = BTensor (Prod d)
  exl = DT (exl . lowerBTensor)
  exr = DT (exr . lowerBTensor)
  diagonal = DT (BTensor . diagonal)
