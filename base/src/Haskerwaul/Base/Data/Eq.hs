{-# LANGUAGE Safe #-}

module Haskerwaul.Base.Data.Eq
  ( Eq,
    (==),
    (/=),
  )
where

import Data.Eq (Eq)
import Haskerwaul.Relation.Equality ((==))
import Haskerwaul.Relation.Inequality ((/=))
