{-# LANGUAGE Safe #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Haskerwaul.Relation.Inequality where

import Data.Bool (Bool)
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Void (Void)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import Haskerwaul.Object
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Topos.Elementary
import Numeric.Natural (Natural)
import Prelude (Double, Float, Integer)
import qualified Prelude as Base

-- | [nLab](https://ncatlab.org/nlab/show/inequality+relation)
--
-- = laws
--   [irreflexive]: @x `/=` x == false@
--   [symmetric]: @x `/=` y ==> y `/=` x@
class (Ob c a) => InequalityRelation c a where
  ne :: HomogeneousRelation c a

instance InequalityRelation (->) () where
  ne ((), ()) = Base.False

instance InequalityRelation (->) Void where
  ne = uncurry $ \case {}

instance InequalityRelation (->) Bool where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Natural where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Int where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Int8 where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Int16 where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Int32 where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Int64 where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Integer where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Word where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Word8 where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Word16 where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Word32 where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Word64 where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Float where
  ne = uncurry (Base./=)

instance InequalityRelation (->) Double where
  ne = uncurry (Base./=)

-- * operators

(/=) :: (ElementaryTopos c, InequalityRelation c a) => a `c` Exp c a (Class c)
(/=) = curry ne
