{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Object.Terminal where

import Data.Constraint (Dict (..), (:-) (..))
import Data.Functor.Const (Const (..))
import Data.Kind (Constraint, Type)
#if MIN_VERSION_base(4, 17, 0)
import Data.Type.Equality (type (~))
#endif
import Haskerwaul.Constraint
import Haskerwaul.Object
import Haskerwaul.Semicategory
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural

-- | There are multiple classes (see
--  `Haskerwaul.Category.Monoidal.Cartesian.CartesianMonoidalCategory` or
--  `Haskerwaul.Category.Pointed.PointedCategory`) that have terminal objects.
--   This just gives us a common class for them to extend.
class (Ob c (TerminalObject c)) => HasTerminalObject (c :: ok -> ok -> Type) where
  -- | [nLab](https://ncatlab.org/nlab/show/terminal+object)
  type TerminalObject c :: ok

  (!) :: (Ob c x) => x `c` TerminalObject c

instance HasTerminalObject (->) where
  type TerminalObject (->) = ()
  (!) _ = ()

instance HasTerminalObject (:-) where
  type TerminalObject (:-) = (() :: Constraint)
  (!) = Sub Dict

instance HasTerminalObject (NaturalTransformation c (:-)) where
  type TerminalObject (NaturalTransformation c (:-)) = All
  (!) = NT (Sub Dict)

-- |
--
--  __TODO__: Generalizing this to @(d `~` (->), `HasTerminalObject` d) =>@
--            leads to conflicting family instances with the
--            @`NaturalTransformation` c (`:-`)@ instance above.
instance HasTerminalObject (NaturalTransformation c (->)) where
  type TerminalObject (NaturalTransformation c (->)) = Const ()

  -- TODO: Replace this with `NT (first (!))`, but it currently causes an import
  --       cycle.
  (!) = NT (Const . (!))

instance
  (d ~ (->), HasTerminalObject d) =>
  HasTerminalObject (DinaturalTransformation d)
  where
  type TerminalObject (DinaturalTransformation d) = BConst (TerminalObject d)
  (!) = DT (BConst . (!))
