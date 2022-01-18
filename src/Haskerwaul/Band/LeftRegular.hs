{-# language UndecidableSuperClasses #-}

module Haskerwaul.Band.LeftRegular
  ( module Haskerwaul.Band.LeftRegular
  -- * extended modules
  , module Haskerwaul.Band
  ) where

import           Data.Bool (Bool)
import           Data.Int (Int, Int8, Int16, Int32, Int64)
import           Data.Word (Word, Word8, Word16, Word32, Word64)
import           Numeric.Natural (Natural)
import           Prelude (Integer)

import Haskerwaul.Band
import Haskerwaul.Lattice.Components

-- |
--
-- = references
--
-- - [nLab](http://ncatlab.org/nlab/show/rectangular%20band#some_ramifications)
class Band c t a => LeftRegularBand c t a

instance LeftRegularBand (->) (,) (Join Bool)
instance LeftRegularBand (->) (,) (Meet Bool)

instance LeftRegularBand (->) (,) (Join Int)
instance LeftRegularBand (->) (,) (Meet Int)

instance LeftRegularBand (->) (,) (Join Int8)
instance LeftRegularBand (->) (,) (Meet Int8)

instance LeftRegularBand (->) (,) (Join Int16)
instance LeftRegularBand (->) (,) (Meet Int16)

instance LeftRegularBand (->) (,) (Join Int32)
instance LeftRegularBand (->) (,) (Meet Int32)

instance LeftRegularBand (->) (,) (Join Int64)
instance LeftRegularBand (->) (,) (Meet Int64)

instance LeftRegularBand (->) (,) (Join Integer)
instance LeftRegularBand (->) (,) (Meet Integer)

instance LeftRegularBand (->) (,) (Join Natural)
instance LeftRegularBand (->) (,) (Meet Natural)

instance LeftRegularBand (->) (,)       ()
instance LeftRegularBand (->) (,) (Join ())
instance LeftRegularBand (->) (,) (Meet ())

instance LeftRegularBand (->) (,) (Join Word)
instance LeftRegularBand (->) (,) (Meet Word)

instance LeftRegularBand (->) (,) (Join Word8)
instance LeftRegularBand (->) (,) (Meet Word8)

instance LeftRegularBand (->) (,) (Join Word16)
instance LeftRegularBand (->) (,) (Meet Word16)

instance LeftRegularBand (->) (,) (Join Word32)
instance LeftRegularBand (->) (,) (Meet Word32)

instance LeftRegularBand (->) (,) (Join Word64)
instance LeftRegularBand (->) (,) (Meet Word64)
