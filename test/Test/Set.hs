{-# language OverloadedStrings #-}

module Test.Set where

import Control.Applicative
import Data.Bool (Bool)
import Data.List (concat)
import Data.Int (Int, Int8)
import Data.Monoid (Sum)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Prelude (Char)

import Haskerwaul hiding (pure)
import Haskerwaul.Hedgehog.Applied.Set

setProperties :: [Hedgehog.Group]
setProperties =
  [ Group
    "(->) -- topos under (,), distributive under (,) Either, colax-distributive under (,) (,)"
    ( concat
      [ semigroupalCategory_lawsFn "product" genTuple
      , semigroupalCategory_lawsFn "coproduct" genEither
      ])
  , Group
    "Int8 forms a semiring under (->) (,)"
    (monoid_lawsTup "(,)" (pure <$> Gen.enumBounded :: Gen (Sum Int8)))
  , Group
    "all types are monoids under (->) Either"
    (monoid_lawsEither "Int" (Gen.enumBounded :: Gen Int)
     <> monoid_lawsEither "Bool" (Gen.enumBounded :: Gen Bool) 
     <> monoid_lawsEither "Char" (Gen.enumBounded :: Gen Char))
  ]
