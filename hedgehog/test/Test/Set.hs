{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module Test.Set where

import Data.Containers.ListUtils
import Data.List (concat)
import Haskerwaul
import Haskerwaul.Hedgehog.Applied.Set
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)
import Prelude (Integer, fst, (<$>))

setProperties :: [Hedgehog.Group]
setProperties =
  [ Group
      "(->) -- topos under (,), distributive under (,) Either, colax-distributive under (,) (,)"
      ( nubOrdOn fst
          $ semigroupalCategory_lawsFn "product" genTuple
          <> semigroupalCategory_lawsFn "coproduct" genEither
      ),
    Group
      "monoids under (->) (,)"
      (nubOrdOn fst $ monoid_lawsTup "String" (Gen.string (Range.linear 0 9_999) Gen.unicodeAll)),
    Group
      "various lattices"
      ( nubOrdOn fst
          $ concat
            [ boundedLattice_lawsTup "()" (pure ()),
              boundedLattice_lawsTup "Bool" Gen.bool,
              boundedLattice_lawsTup "Int" (Gen.int Range.linearBounded),
              boundedLattice_lawsTup "Int8" (Gen.int8 Range.linearBounded),
              boundedLattice_lawsTup "Int16" (Gen.int16 Range.linearBounded),
              boundedLattice_lawsTup "Int32" (Gen.int32 Range.linearBounded),
              boundedLattice_lawsTup "Int64" (Gen.int64 Range.linearBounded),
              lattice_lawsTup "Integer" (Gen.integral_ (Range.linear (-99_999_999_999) 99_999_999_999) :: Gen Integer),
              lattice_lawsTup "Natural" (Gen.integral (Range.linear 0 99_999_999_999) :: Gen Natural),
              boundedLattice_lawsTup "Word" (Gen.word Range.linearBounded),
              boundedLattice_lawsTup "Word8" (Gen.word8 Range.linearBounded),
              boundedLattice_lawsTup "Word16" (Gen.word16 Range.linearBounded),
              boundedLattice_lawsTup "Word32" (Gen.word32 Range.linearBounded),
              boundedLattice_lawsTup "Word64" (Gen.word64 Range.linearBounded)
            ]
      ),
    Group
      "Integral numbers form a rig under (->) (,)"
      ( nubOrdOn fst
          $ concat
            [ rig_lawsTup "()" (pure ()),
              rig_lawsTup "Int" (Gen.int Range.linearBounded),
              rig_lawsTup "Int8" (Gen.int8 Range.linearBounded),
              rig_lawsTup "Int16" (Gen.int16 Range.linearBounded),
              rig_lawsTup "Int32" (Gen.int32 Range.linearBounded),
              rig_lawsTup "Int64" (Gen.int64 Range.linearBounded),
              rig_lawsTup "Integer" (Gen.integral_ (Range.linear (-99_999_999_999) 99_999_999_999) :: Gen Integer),
              rig_lawsTup "Natural" (Gen.integral (Range.linear 0 99_999_999_999) :: Gen Natural),
              -- rig_lawsTup "Join Natural" (map pure (Gen.integral (Range.linear 0 99_999_999_999)) :: Gen (Join Natural))
              rig_lawsTup "Word" (Gen.word Range.linearBounded),
              rig_lawsTup "Word8" (Gen.word8 Range.linearBounded),
              rig_lawsTup "Word16" (Gen.word16 Range.linearBounded),
              rig_lawsTup "Word32" (Gen.word32 Range.linearBounded),
              rig_lawsTup "Word64" (Gen.word64 Range.linearBounded)
            ]
      ),
    Group
      "all types are commutative monoids under (->) Either"
      ( nubOrdOn fst
          $ concat
            [ commutativeMonoid_lawsEither "Int" (Gen.int Range.linearBounded),
              commutativeMonoid_lawsEither "Bool" Gen.bool,
              commutativeMonoid_lawsEither "Char" Gen.unicodeAll
            ]
      )
  ]
