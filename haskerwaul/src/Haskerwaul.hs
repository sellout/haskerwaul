{-# LANGUAGE Safe #-}

-- | This re-exports everything in the library, providing a unified model of
--   category theory.
--
--   In general, you shouldn't import this. This mostly ensures that all the
--   pieces play well together. In practice, importing the "strongest" modules
--   you need is enough, as they should each re-export all the modules they
--   extend.
module Haskerwaul
  ( -- * adjunctions

    --  module Haskerwaul.Functor.Adjoint
    module Haskerwaul.GaloisConnection,

    -- * algebras
    module Haskerwaul.Algebra.Boolean,
    module Haskerwaul.Algebra.Heyting,
    module Haskerwaul.Algebra.Heyting.Complete,

    -- * bands
    module Haskerwaul.Band,
    module Haskerwaul.Band.LeftRegular,
    module Haskerwaul.Band.Rectangular,

    -- * categories
    module Haskerwaul.Category,
    module Haskerwaul.Category.Bicartesian,
    module Haskerwaul.Category.Boolean,
    module Haskerwaul.Category.Coherent,
    module Haskerwaul.Category.CoKleisli,
    module Haskerwaul.Category.Complete.Finitely,
    module Haskerwaul.Category.Concrete,
    module Haskerwaul.Category.Distributive,
    module Haskerwaul.Category.Distributive.Linearly,
    module Haskerwaul.Category.Duoidal,
    module Haskerwaul.Category.Duoidal.Normal,
    module Haskerwaul.Category.Heyting,
    module Haskerwaul.Category.Hypergraph,
    module Haskerwaul.Category.Kleisli,
    module Haskerwaul.Category.LocallyCartesian,
    module Haskerwaul.Category.Opposite,
    module Haskerwaul.Category.Over,
    module Haskerwaul.Category.Pointed,
    module Haskerwaul.Category.Presheaves,
    module Haskerwaul.Category.Product,
    module Haskerwaul.Category.Regular,
    module Haskerwaul.Category.Ribbon,
    module Haskerwaul.Category.Rig,
    module Haskerwaul.Category.Rig.ColaxDistributive,
    module Haskerwaul.Category.Rig.ColaxDistributive.Left,
    module Haskerwaul.Category.Rig.ColaxDistributive.Right,
    module Haskerwaul.Category.Semigroupal,
    module Haskerwaul.Category.Small,
    module Haskerwaul.Category.Small.Locally,
    module Haskerwaul.Category.Terminal,
    module Haskerwaul.Category.Under,

    -- ** closed categories
    module Haskerwaul.Category.Closed,
    module Haskerwaul.Category.Closed.Bicartesian,
    module Haskerwaul.Category.Closed.Cartesian,
    module Haskerwaul.Category.Closed.Compact,
    module Haskerwaul.Category.Closed.Symmetric,

    -- ** monoidal categories
    module Haskerwaul.Category.Monoidal,
    module Haskerwaul.Category.Monoidal.Balanced,
    module Haskerwaul.Category.Monoidal.Braided,
    module Haskerwaul.Category.Monoidal.Cartesian,
    module Haskerwaul.Category.Monoidal.Cartesian.Traced,
    module Haskerwaul.Category.Monoidal.Closed,
    module Haskerwaul.Category.Monoidal.Closed.Cartesian,
    module Haskerwaul.Category.Monoidal.Cocartesian,
    module Haskerwaul.Category.Monoidal.Distributive,
    module Haskerwaul.Category.Monoidal.Rigid,
    module Haskerwaul.Category.Monoidal.Rigid.Left,
    module Haskerwaul.Category.Monoidal.Rigid.Right,
    module Haskerwaul.Category.Monoidal.Symmetric,
    module Haskerwaul.Category.Monoidal.Traced,
    module Haskerwaul.Category.MonoidalUnit,

    -- * cones
    module Haskerwaul.Cocone,
    module Haskerwaul.Cone,
    module Haskerwaul.Day,

    -- * dioids
    module Haskerwaul.Dioid,
    module Haskerwaul.Dioid.Commutative,

    -- * duoids
    module Haskerwaul.Duoid,

    -- * embeddings
    module Haskerwaul.Embedding.Yoneda,

    -- * extensions
    module Haskerwaul.Extension.Kan.Left,
    module Haskerwaul.Extension.Kan.Right,

    -- * fields
    module Haskerwaul.Field,
    module Haskerwaul.Field.Totalized,
    module Haskerwaul.Field.Totalized.One,
    module Haskerwaul.Field.Totalized.Zero,
    module Haskerwaul.Skewfield,

    -- * functors
    module Haskerwaul.Bifunctor,
    module Haskerwaul.Endofunctor,
    module Haskerwaul.Functor,
    module Haskerwaul.Functor.Closed.Cartesian,
    module Haskerwaul.Functor.Closed.Lax,
    module Haskerwaul.Functor.Closed.Strong,
    module Haskerwaul.Functor.Contravariant,
    module Haskerwaul.Functor.Diagonal,
    module Haskerwaul.Functor.Faithful,
    module Haskerwaul.Functor.Faithful.Full,
    module Haskerwaul.Functor.Full,
    module Haskerwaul.Functor.Hom,
    module Haskerwaul.Functor.Hom.Internal,
    module Haskerwaul.Functor.Strong,
    module Haskerwaul.Presheaf,
    module Haskerwaul.Profunctor,
    module Haskerwaul.Semifunctor,
    module Haskerwaul.Semipresheaf,

    -- ** monoidal functors
    module Haskerwaul.Functor.Monoidal.Closed,
    module Haskerwaul.Functor.Monoidal.Lax,
    module Haskerwaul.Functor.Monoidal.Oplax,
    module Haskerwaul.Functor.Monoidal.Strong,

    -- * groups
    module Haskerwaul.Group,
    module Haskerwaul.Group.Abelian,
    module Haskerwaul.Groupoid,

    -- * hemirings
    module Haskerwaul.Hemiring,
    module Haskerwaul.Hemiring.Near,

    -- * lattices
    module Haskerwaul.Lattice,
    module Haskerwaul.Lattice.Bounded,
    module Haskerwaul.Lattice.Complemented,
    module Haskerwaul.Lattice.Complemented.Uniquely,
    module Haskerwaul.Lattice.Distributive,
    module Haskerwaul.Lattice.Distributive.Bounded,
    module Haskerwaul.Lattice.Modular,
    module Haskerwaul.Lattice.Orthocomplemented,
    module Haskerwaul.Lattice.Orthomodular,

    -- * laws
    module Haskerwaul.Law,
    module Haskerwaul.Law.Alternativity.Left,
    module Haskerwaul.Law.Alternativity.Right,
    module Haskerwaul.Law.Associativity,
    module Haskerwaul.Law.Asymmetry,
    module Haskerwaul.Law.Commutativity,
    module Haskerwaul.Law.Distributive.Left,
    module Haskerwaul.Law.Distributive.Right,
    module Haskerwaul.Law.Distributive.Self.Left,
    module Haskerwaul.Law.Distributive.Self.Right,
    module Haskerwaul.Law.Flexibility,
    module Haskerwaul.Law.Homomorphism.Identity,
    module Haskerwaul.Law.Homomorphism.Magma,
    module Haskerwaul.Law.Idempotency,
    module Haskerwaul.Law.Identity.Left,
    module Haskerwaul.Law.Identity.Right,
    module Haskerwaul.Law.Interchange,
    module Haskerwaul.Law.Modularity,
    module Haskerwaul.Law.Quotient.Left,
    module Haskerwaul.Law.Quotient.Right,
    module Haskerwaul.Law.Reflexivity,
    module Haskerwaul.Law.Symmetry,

    -- * loops
    module Haskerwaul.Loop,

    -- * magmas
    module Haskerwaul.Magma,
    module Haskerwaul.Magma.Commutative,
    module Haskerwaul.Magma.Invertible,
    module Haskerwaul.Magma.Invertible.Commutative,
    module Haskerwaul.Magma.Invertible.Left,
    module Haskerwaul.Magma.Invertible.Right,
    module Haskerwaul.Magma.Unital,

    -- * meadows
    module Haskerwaul.Meadow,
    module Haskerwaul.Meadow.Cancellation,
    module Haskerwaul.Meadow.NonInvolutive,
    module Haskerwaul.Meadow.NonTrivial,

    -- * modules
    module Haskerwaul.Module,
    module Haskerwaul.Module.Left,
    module Haskerwaul.Module.Right,

    -- * monads
    module Haskerwaul.Comonad,
    module Haskerwaul.Monad,
    module Haskerwaul.Monad.Codensity,
    module Haskerwaul.Monad.Frobenius,

    -- * monoids
    module Haskerwaul.Bimonoid,
    module Haskerwaul.Comonoid,
    module Haskerwaul.Monoid,
    module Haskerwaul.Monoid.Commutative,
    module Haskerwaul.Monoid.Commutative.Monus,
    module Haskerwaul.Monoid.Frobenius,
    module Haskerwaul.Monoid.Frobenius.Commutative,
    module Haskerwaul.Monoid.Frobenius.Commutative.Special,
    module Haskerwaul.Monoid.Frobenius.Special,
    module Haskerwaul.Monoid.Graphic,
    module Haskerwaul.Monoid.Hopf,

    -- * morphisms
    module Haskerwaul.Automorphism,
    module Haskerwaul.Endomorphism,
    module Haskerwaul.Isomorphism,
    module Haskerwaul.Isomorphism.Natural,

    -- * objects
    module Haskerwaul.Object,
    module Haskerwaul.Object.Dualizable,
    module Haskerwaul.Object.Dualizable.Left,
    module Haskerwaul.Object.Dualizable.Right,
    module Haskerwaul.Object.Exponential,
    module Haskerwaul.Object.Free,
    module Haskerwaul.Object.Initial,
    -- , module Haskerwaul.Object.Integers
    module Haskerwaul.Object.Invertible,
    module Haskerwaul.Object.NaturalNumbers,
    module Haskerwaul.Object.Terminal,

    -- * orders
    module Haskerwaul.Order.Canonical,
    module Haskerwaul.Order.Linear,
    module Haskerwaul.Order.Partial,
    module Haskerwaul.Order.Prefix,
    module Haskerwaul.Order.Total,
    module Haskerwaul.Preorder,
    module Haskerwaul.Quasiorder,

    -- * pullbacks
    module Haskerwaul.Pullback,
    module Haskerwaul.Pushout,

    -- * quasigroups
    module Haskerwaul.Quasigroup,
    module Haskerwaul.Quasigroup.Commutative,
    module Haskerwaul.Quasigroup.Invertible,
    module Haskerwaul.Quasigroup.Invertible.Commutative,
    module Haskerwaul.Quasigroup.Left,
    module Haskerwaul.Quasigroup.Right,

    -- * racks
    module Haskerwaul.Quandle,
    module Haskerwaul.Rack,
    module Haskerwaul.Shelf,
    module Haskerwaul.Shelf.Left,
    module Haskerwaul.Shelf.Right,

    -- * relations
    module Haskerwaul.Congruence,
    module Haskerwaul.Negation,
    module Haskerwaul.Relation.Apartness,
    module Haskerwaul.Relation.Apartness.Tight,
    module Haskerwaul.Relation.Binary,
    module Haskerwaul.Relation.Dependency,
    module Haskerwaul.Relation.Equality,
    module Haskerwaul.Relation.Equality.Decidable,
    module Haskerwaul.Relation.Equality.Stable,
    module Haskerwaul.Relation.Equivalence,
    module Haskerwaul.Relation.Equivalence.Partial,
    module Haskerwaul.Relation.Homogeneous,
    module Haskerwaul.Relation.Inequality,
    module Haskerwaul.Relation.Inequality.Denial,
    module Haskerwaul.Relation.Inequality.Tight,
    module Haskerwaul.Relation.Nullary,
    module Haskerwaul.Relation.Tolerance,
    module Haskerwaul.Relation.Unary,

    -- * residuals
    module Haskerwaul.Residual,
    module Haskerwaul.Residual.Left,
    module Haskerwaul.Residual.Right,

    -- * rigs
    module Haskerwaul.Rig,
    module Haskerwaul.Rig.Idempotent,
    module Haskerwaul.Rig.Monus,

    -- * rings
    module Haskerwaul.Ring,
    module Haskerwaul.Ring.Boolean,
    module Haskerwaul.Ring.Commutative,
    module Haskerwaul.Ring.Nonunital,

    -- * semigroups
    module Haskerwaul.Semicategory,
    module Haskerwaul.Semigroup,
    module Haskerwaul.Semigroup.Commutative,
    module Haskerwaul.Semigroup.Inverse,

    -- * semilattices
    module Haskerwaul.Semilattice,
    module Haskerwaul.Semilattice.Bounded,

    -- * semirings
    module Haskerwaul.Semiring,
    module Haskerwaul.Semiring.Idempotent,
    module Haskerwaul.Semiring.MaxPlus,
    module Haskerwaul.Semiring.MinPlus,
    module Haskerwaul.Semiring.Near,
    module Haskerwaul.Semiring.Pre,
    module Haskerwaul.Semiring.Pre.Near,
    module Haskerwaul.Semiring.Tropical,

    -- * spans
    module Haskerwaul.Cospan,
    module Haskerwaul.Span,

    -- * subcategories
    module Haskerwaul.Subcategory,
    module Haskerwaul.Subcategory.Full,
    module Haskerwaul.Subcategory.Wide,

    -- * topoi (toposes)
    module Haskerwaul.Topos.Elementary,
    module Haskerwaul.Topos.Grothendieck,

    -- * natural transformations
    module Haskerwaul.Transformation.Dinatural,
    module Haskerwaul.Transformation.Natural,
  )
where

import Haskerwaul.Algebra.Boolean
import Haskerwaul.Algebra.Heyting
import Haskerwaul.Algebra.Heyting.Complete
import Haskerwaul.Automorphism
import Haskerwaul.Band
import Haskerwaul.Band.LeftRegular
import Haskerwaul.Band.Rectangular
import Haskerwaul.Bifunctor
import Haskerwaul.Bimonoid
import Haskerwaul.Category
import Haskerwaul.Category.Bicartesian
import Haskerwaul.Category.Boolean
import Haskerwaul.Category.Closed
import Haskerwaul.Category.Closed.Bicartesian
import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Category.Closed.Compact
import Haskerwaul.Category.Closed.Symmetric
import Haskerwaul.Category.CoKleisli
import Haskerwaul.Category.Coherent
import Haskerwaul.Category.Complete.Finitely
import Haskerwaul.Category.Concrete
import Haskerwaul.Category.Distributive
import Haskerwaul.Category.Distributive.Linearly
import Haskerwaul.Category.Duoidal
import Haskerwaul.Category.Duoidal.Normal
import Haskerwaul.Category.Heyting
import Haskerwaul.Category.Hypergraph
import Haskerwaul.Category.Kleisli
import Haskerwaul.Category.LocallyCartesian
import Haskerwaul.Category.Monoidal
import Haskerwaul.Category.Monoidal.Balanced
import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Monoidal.Cartesian.Traced
import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Category.Monoidal.Closed.Cartesian
import Haskerwaul.Category.Monoidal.Cocartesian
import Haskerwaul.Category.Monoidal.Distributive
import Haskerwaul.Category.Monoidal.Rigid
import Haskerwaul.Category.Monoidal.Rigid.Left
import Haskerwaul.Category.Monoidal.Rigid.Right
import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Category.Monoidal.Traced
import Haskerwaul.Category.MonoidalUnit
import Haskerwaul.Category.Opposite
import Haskerwaul.Category.Over
import Haskerwaul.Category.Pointed
import Haskerwaul.Category.Presheaves
import Haskerwaul.Category.Product
import Haskerwaul.Category.Regular
import Haskerwaul.Category.Ribbon
import Haskerwaul.Category.Rig
import Haskerwaul.Category.Rig.ColaxDistributive
import Haskerwaul.Category.Rig.ColaxDistributive.Left
import Haskerwaul.Category.Rig.ColaxDistributive.Right
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Category.Small
import Haskerwaul.Category.Small.Locally
import Haskerwaul.Category.Terminal
import Haskerwaul.Category.Under
import Haskerwaul.Cocone
import Haskerwaul.Comonad
import Haskerwaul.Comonoid
import Haskerwaul.Cone
import Haskerwaul.Congruence
import Haskerwaul.Cospan
import Haskerwaul.Day
import Haskerwaul.Dioid
import Haskerwaul.Dioid.Commutative
import Haskerwaul.Duoid
import Haskerwaul.Embedding.Yoneda
import Haskerwaul.Endofunctor
import Haskerwaul.Endomorphism
import Haskerwaul.Extension.Kan.Left
import Haskerwaul.Extension.Kan.Right
import Haskerwaul.Field
import Haskerwaul.Field.Totalized
import Haskerwaul.Field.Totalized.One
import Haskerwaul.Field.Totalized.Zero
import Haskerwaul.Functor
-- FIXME: causes conflicting exports for `epsilion` with
--       "Haskerwaul.Functor.Monoidal.Strong".
-- import Haskerwaul.Functor.Adjoint
import Haskerwaul.Functor.Closed.Cartesian
import Haskerwaul.Functor.Closed.Lax
import Haskerwaul.Functor.Closed.Strong
import Haskerwaul.Functor.Contravariant
import Haskerwaul.Functor.Diagonal
import Haskerwaul.Functor.Faithful
import Haskerwaul.Functor.Faithful.Full
import Haskerwaul.Functor.Full
import Haskerwaul.Functor.Hom
import Haskerwaul.Functor.Hom.Internal
import Haskerwaul.Functor.Monoidal.Closed
import Haskerwaul.Functor.Monoidal.Lax
import Haskerwaul.Functor.Monoidal.Oplax
import Haskerwaul.Functor.Monoidal.Strong
import Haskerwaul.Functor.Strong
import Haskerwaul.GaloisConnection
import Haskerwaul.Group
import Haskerwaul.Group.Abelian
import Haskerwaul.Groupoid
import Haskerwaul.Hemiring
import Haskerwaul.Hemiring.Near
import Haskerwaul.Isomorphism
import Haskerwaul.Isomorphism.Natural
import Haskerwaul.Lattice
import Haskerwaul.Lattice.Bounded
import Haskerwaul.Lattice.Complemented
import Haskerwaul.Lattice.Complemented.Uniquely
import Haskerwaul.Lattice.Distributive
import Haskerwaul.Lattice.Distributive.Bounded
import Haskerwaul.Lattice.Modular
import Haskerwaul.Lattice.Orthocomplemented
import Haskerwaul.Lattice.Orthomodular
import Haskerwaul.Law
import Haskerwaul.Law.Alternativity.Left
import Haskerwaul.Law.Alternativity.Right
import Haskerwaul.Law.Associativity
import Haskerwaul.Law.Asymmetry
import Haskerwaul.Law.Commutativity
import Haskerwaul.Law.Distributive.Left
import Haskerwaul.Law.Distributive.Right
import Haskerwaul.Law.Distributive.Self.Left
import Haskerwaul.Law.Distributive.Self.Right
import Haskerwaul.Law.Flexibility
import Haskerwaul.Law.Homomorphism.Identity
import Haskerwaul.Law.Homomorphism.Magma
import Haskerwaul.Law.Idempotency
import Haskerwaul.Law.Identity.Left
import Haskerwaul.Law.Identity.Right
import Haskerwaul.Law.Interchange
import Haskerwaul.Law.Modularity
import Haskerwaul.Law.Quotient.Left
import Haskerwaul.Law.Quotient.Right
import Haskerwaul.Law.Reflexivity
import Haskerwaul.Law.Symmetry
import Haskerwaul.Loop
import Haskerwaul.Loop.Commutative
import Haskerwaul.Magma
import Haskerwaul.Magma.Commutative
import Haskerwaul.Magma.Invertible
import Haskerwaul.Magma.Invertible.Commutative
import Haskerwaul.Magma.Invertible.Left
import Haskerwaul.Magma.Invertible.Right
import Haskerwaul.Magma.Unital
import Haskerwaul.Meadow
import Haskerwaul.Meadow.Cancellation
import Haskerwaul.Meadow.NonInvolutive
import Haskerwaul.Meadow.NonTrivial
import Haskerwaul.Module
import Haskerwaul.Module.Left
import Haskerwaul.Module.Right
import Haskerwaul.Monad
import Haskerwaul.Monad.Codensity
import Haskerwaul.Monad.Frobenius
import Haskerwaul.Monoid
import Haskerwaul.Monoid.Commutative
import Haskerwaul.Monoid.Commutative.Monus
import Haskerwaul.Monoid.Frobenius
import Haskerwaul.Monoid.Frobenius.Commutative
import Haskerwaul.Monoid.Frobenius.Commutative.Special
import Haskerwaul.Monoid.Frobenius.Special
import Haskerwaul.Monoid.Graphic
import Haskerwaul.Monoid.Hopf
import Haskerwaul.Negation
import Haskerwaul.Object
import Haskerwaul.Object.Dualizable
import Haskerwaul.Object.Dualizable.Left
import Haskerwaul.Object.Dualizable.Right
import Haskerwaul.Object.Exponential
import Haskerwaul.Object.Free
import Haskerwaul.Object.Initial
-- FIXME: causes conflicting exports in various places.
-- import Haskerwaul.Object.Integers
import Haskerwaul.Object.Invertible
import Haskerwaul.Object.NaturalNumbers
import Haskerwaul.Object.Terminal
import Haskerwaul.Order.Canonical
import Haskerwaul.Order.Linear
import Haskerwaul.Order.Partial
import Haskerwaul.Order.Prefix
import Haskerwaul.Order.Total
import Haskerwaul.Preorder
import Haskerwaul.Presheaf
import Haskerwaul.Profunctor
import Haskerwaul.Pullback
import Haskerwaul.Pushout
import Haskerwaul.Quandle
import Haskerwaul.Quasigroup
import Haskerwaul.Quasigroup.Commutative
import Haskerwaul.Quasigroup.Invertible
import Haskerwaul.Quasigroup.Invertible.Commutative
import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right
import Haskerwaul.Quasiorder
import Haskerwaul.Rack
import Haskerwaul.Relation.Apartness
import Haskerwaul.Relation.Apartness.Tight
import Haskerwaul.Relation.Binary
import Haskerwaul.Relation.Dependency
import Haskerwaul.Relation.Equality
import Haskerwaul.Relation.Equality.Decidable
import Haskerwaul.Relation.Equality.Stable
import Haskerwaul.Relation.Equivalence
import Haskerwaul.Relation.Equivalence.Partial
import Haskerwaul.Relation.Homogeneous
import Haskerwaul.Relation.Inequality
import Haskerwaul.Relation.Inequality.Denial
import Haskerwaul.Relation.Inequality.Tight
import Haskerwaul.Relation.Nullary
import Haskerwaul.Relation.Tolerance
import Haskerwaul.Relation.Unary
import Haskerwaul.Residual
import Haskerwaul.Residual.Left
import Haskerwaul.Residual.Right
import Haskerwaul.Rig
import Haskerwaul.Rig.Idempotent
import Haskerwaul.Rig.Monus
import Haskerwaul.Ring
import Haskerwaul.Ring.Boolean
import Haskerwaul.Ring.Commutative
import Haskerwaul.Ring.Nonunital
import Haskerwaul.Semicategory
import Haskerwaul.Semifunctor
import Haskerwaul.Semigroup
import Haskerwaul.Semigroup.Commutative
import Haskerwaul.Semigroup.Inverse
import Haskerwaul.Semilattice
import Haskerwaul.Semilattice.Bounded
import Haskerwaul.Semipresheaf
import Haskerwaul.Semiring
import Haskerwaul.Semiring.Idempotent
import Haskerwaul.Semiring.MaxPlus
import Haskerwaul.Semiring.MinPlus
import Haskerwaul.Semiring.Near
import Haskerwaul.Semiring.Pre
import Haskerwaul.Semiring.Pre.Near
import Haskerwaul.Semiring.Tropical
import Haskerwaul.Shelf
import Haskerwaul.Shelf.Left
import Haskerwaul.Shelf.Right
import Haskerwaul.Skewfield
import Haskerwaul.Span
import Haskerwaul.Subcategory
import Haskerwaul.Subcategory.Full
import Haskerwaul.Subcategory.Wide
import Haskerwaul.Topos.Elementary
import Haskerwaul.Topos.Grothendieck
import Haskerwaul.Transformation.Dinatural
import Haskerwaul.Transformation.Natural
