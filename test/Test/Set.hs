{-# language OverloadedStrings #-}

module Test.Set where

import Data.List (concat)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)
import Prelude (Integer)

import Haskerwaul
import Haskerwaul.Hedgehog.Applied.Set

setProperties :: [Hedgehog.Group]
setProperties =
  [ Group
    "(->) -- topos under (,), distributive under (,) Either, colax-distributive under (,) (,)"
    ( concat
      [ semigroupalCategory_lawsFn "product" genTuple
      , semigroupalCategory_lawsFn "coproduct" genEither
      ])
  , Group "monoids under (->) (,)"
    (monoid_lawsTup "String" (Gen.string (Range.linear 0 9999) Gen.unicodeAll))
  , Group
    "various lattices"
    ( concat
      [ boundedLattice_lawsTup "()" (pure ())
      , boundedLattice_lawsTup "Bool" Gen.bool
      , boundedLattice_lawsTup "Int" (Gen.int Range.linearBounded)
      , boundedLattice_lawsTup "Int8" (Gen.int8 Range.linearBounded)
      , boundedLattice_lawsTup "Int16" (Gen.int16 Range.linearBounded)
      , boundedLattice_lawsTup "Int32" (Gen.int32 Range.linearBounded)
      , boundedLattice_lawsTup "Int64" (Gen.int64 Range.linearBounded)
      , lattice_lawsTup "Integer" (Gen.integral_ (Range.linear (-99999999999) 99999999999) :: Gen Integer)
      , lattice_lawsTup "Natural" (Gen.integral (Range.linear 0 99999999999) :: Gen Natural)
      , boundedLattice_lawsTup "Word" (Gen.word Range.linearBounded)
      , boundedLattice_lawsTup "Word8" (Gen.word8 Range.linearBounded)
      , boundedLattice_lawsTup "Word16" (Gen.word16 Range.linearBounded)
      , boundedLattice_lawsTup "Word32" (Gen.word32 Range.linearBounded)
      , boundedLattice_lawsTup "Word64" (Gen.word64 Range.linearBounded)
      ])
  , Group
    "Integral numbers form a rig under (->) (,)"
    ( concat
      [ rig_lawsTup "()" (pure ())
      , rig_lawsTup "Int" (Gen.int Range.linearBounded)
      , rig_lawsTup "Int8" (Gen.int8 Range.linearBounded)
      , rig_lawsTup "Int16" (Gen.int16 Range.linearBounded)
      , rig_lawsTup "Int32" (Gen.int32 Range.linearBounded)
      , rig_lawsTup "Int64" (Gen.int64 Range.linearBounded)
      , rig_lawsTup "Integer" (Gen.integral_ (Range.linear (-99999999999) 99999999999) :: Gen Integer)
      , rig_lawsTup "Natural" (Gen.integral (Range.linear 0 99999999999) :: Gen Natural)
      -- , rig_lawsTup "Join Natural" (map pure (Gen.integral (Range.linear 0 99999999999)) :: Gen (Join Natural))
      , rig_lawsTup "Word" (Gen.word Range.linearBounded)
      , rig_lawsTup "Word8" (Gen.word8 Range.linearBounded)
      , rig_lawsTup "Word16" (Gen.word16 Range.linearBounded)
      , rig_lawsTup "Word32" (Gen.word32 Range.linearBounded)
      , rig_lawsTup "Word64" (Gen.word64 Range.linearBounded)
      ])
  , Group
    "all types are commutative monoids under (->) Either"
    ( concat
      [ commutativeMonoid_lawsEither "Int" (Gen.int Range.linearBounded)
      , commutativeMonoid_lawsEither "Bool" Gen.bool
      , commutativeMonoid_lawsEither "Char" Gen.unicodeAll
      ])
  ]
