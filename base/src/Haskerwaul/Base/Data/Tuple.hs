{-# LANGUAGE Safe #-}

module Haskerwaul.Base.Data.Tuple
  ( curry,
    uncurry,
  )
where

import qualified Haskerwaul.Category.Monoidal.Closed as Haskerwaul
import Haskerwaul.Object

curry :: (Ob (->) x, Ob (->) y, Ob (->) z) => ((x, y) -> z) -> x -> y -> z
curry = Haskerwaul.curry

uncurry :: (Ob (->) x, Ob (->) y, Ob (->) z) => (x -> y -> z) -> (x, y) -> z
uncurry = Haskerwaul.uncurry
