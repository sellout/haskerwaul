{-# LANGUAGE Safe #-}

module Haskerwaul.Base.Data.Semigroup
  ( Semigroup,
    (<>),
  )
where

import Data.Semigroup (Semigroup)
import Haskerwaul.Category.Monoidal.Closed.Cartesian ((<>))
