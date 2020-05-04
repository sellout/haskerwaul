-- | This re-exports everything in the library, providing a unified model of
--   category theory.
--
--   In general, you shouldn't import this. This mostly ensures that all the
--   pieces play well together. In practice, importing the "strongest" modules
--   you need is enough, as they should each re-export all the modules they
--   extend.
module Haskerwaul
  (
--  module Haskerwaul.Adjunction
  -- * algebras
    module Haskerwaul.Algebra.Boolean
  , module Haskerwaul.Algebra.Heyting
  , module Haskerwaul.Algebra.Heyting.Complete
  -- * categories
  , module Haskerwaul.Category
  , module Haskerwaul.Category.Bicartesian
  , module Haskerwaul.Category.CoKleisli
  , module Haskerwaul.Category.Concrete
  , module Haskerwaul.Category.Distributive
  , module Haskerwaul.Category.Duoidal
  , module Haskerwaul.Category.Duoidal.Normal
  , module Haskerwaul.Category.Hypergraph
  , module Haskerwaul.Category.Kleisli
  , module Haskerwaul.Category.Opposite
  , module Haskerwaul.Category.Pointed
  , module Haskerwaul.Category.Product
  , module Haskerwaul.Category.Rig
  , module Haskerwaul.Category.Semigroupal
  , module Haskerwaul.Category.Terminal
  -- ** closed categories
  , module Haskerwaul.Category.Closed
  , module Haskerwaul.Category.Closed.Bicartesian
  , module Haskerwaul.Category.Closed.Cartesian
  , module Haskerwaul.Category.Closed.Compact
  -- ** monoidal categories
  , module Haskerwaul.Category.Monoidal
  , module Haskerwaul.Category.Monoidal.Braided
  , module Haskerwaul.Category.Monoidal.Cartesian
  , module Haskerwaul.Category.Monoidal.Closed
  , module Haskerwaul.Category.Monoidal.Cocartesian
  , module Haskerwaul.Category.Monoidal.Distributive
  , module Haskerwaul.Category.Monoidal.Symmetric
  , module Haskerwaul.Category.Monoidal'
  -- * cones
  , module Haskerwaul.Cocone
  , module Haskerwaul.Cone
  , module Haskerwaul.Day
  , module Haskerwaul.Dioid
  -- * duoids
  , module Haskerwaul.Duoid
  , module Haskerwaul.Duoid.Normal
  -- * embeddings
  , module Haskerwaul.Embedding.Yoneda
  -- * extensions
  , module Haskerwaul.Extension.Kan.Left
  , module Haskerwaul.Extension.Kan.Right
  -- * fields
  , module Haskerwaul.Field
  , module Haskerwaul.Field.Totalized
  , module Haskerwaul.Field.Totalized.One
  , module Haskerwaul.Field.Totalized.Zero
  , module Haskerwaul.Skewfield
  -- * functors
  , module Haskerwaul.Bifunctor
  , module Haskerwaul.Endofunctor
  , module Haskerwaul.Functor
  , module Haskerwaul.Functor.Closed.Cartesian
  , module Haskerwaul.Functor.Closed.Lax
  , module Haskerwaul.Functor.Closed.Strong
  , module Haskerwaul.Functor.Contravariant
  , module Haskerwaul.Functor.Faithful
  , module Haskerwaul.Functor.Faithful.Full
  , module Haskerwaul.Functor.Full
  , module Haskerwaul.Functor.Hom
  , module Haskerwaul.Functor.Hom.Internal
  , module Haskerwaul.Functor.Strong
  , module Haskerwaul.Profunctor
  -- ** monoidal functors
  , module Haskerwaul.Functor.Monoidal.Closed
  , module Haskerwaul.Functor.Monoidal.Lax
  , module Haskerwaul.Functor.Monoidal.Oplax
  , module Haskerwaul.Functor.Monoidal.Strong
  -- * groups
  , module Haskerwaul.Group
  , module Haskerwaul.Group.Abelian
  , module Haskerwaul.Groupoid
  -- * hemirings
  , module Haskerwaul.Hemiring
  , module Haskerwaul.Hemiring.Near
  -- * isomorphisms
  , module Haskerwaul.Automorphism
  , module Haskerwaul.Isomorphism
  , module Haskerwaul.Isomorphism.Natural
  -- * lattices
  , module Haskerwaul.Lattice
  , module Haskerwaul.Lattice.Bounded
  , module Haskerwaul.Lattice.Complemented
  , module Haskerwaul.Lattice.Complemented.Uniquely
  , module Haskerwaul.Lattice.Distributive
  , module Haskerwaul.Lattice.Distributive.Bounded
  , module Haskerwaul.Lattice.Modular
  , module Haskerwaul.Lattice.Orthocomplemented
  , module Haskerwaul.Lattice.Orthomodular
  -- * laws
  , module Haskerwaul.Law
  , module Haskerwaul.Law.Alternativity.Left
  , module Haskerwaul.Law.Alternativity.Right
  , module Haskerwaul.Law.Associativity
  , module Haskerwaul.Law.Asymmetry
  , module Haskerwaul.Law.Commutativity
  , module Haskerwaul.Law.Distributive.Left
  , module Haskerwaul.Law.Distributive.Right
  , module Haskerwaul.Law.Flexibility
  , module Haskerwaul.Law.Homomorphism.Identity
  , module Haskerwaul.Law.Homomorphism.Magma
  , module Haskerwaul.Law.Idempotency
  , module Haskerwaul.Law.Identity.Left
  , module Haskerwaul.Law.Identity.Right
  , module Haskerwaul.Law.Modularity
  , module Haskerwaul.Law.Quotient.Left
  , module Haskerwaul.Law.Quotient.Right
  , module Haskerwaul.Law.Reflexivity
  , module Haskerwaul.Law.Symmetry
  -- * loops
  , module Haskerwaul.Loop
  -- * magmas
  , module Haskerwaul.Magma
  , module Haskerwaul.Magma.Commutative
  , module Haskerwaul.Magma.Idempotent
  , module Haskerwaul.Magma.Unital
  -- * meadows
  , module Haskerwaul.Meadow
  , module Haskerwaul.Meadow.Cancellation
  , module Haskerwaul.Meadow.NonInvolutive
  , module Haskerwaul.Meadow.NonTrivial
  -- * modules
  , module Haskerwaul.Module
  , module Haskerwaul.Module.Left
  , module Haskerwaul.Module.Right
  -- * monads
  , module Haskerwaul.Comonad
  , module Haskerwaul.Monad
  , module Haskerwaul.Monad.Codensity
  , module Haskerwaul.Monad.Frobenius
  -- * monoids
  , module Haskerwaul.Bimonoid
  , module Haskerwaul.Comonoid
  , module Haskerwaul.Monoid
  , module Haskerwaul.Monoid.Commutative
  , module Haskerwaul.Monoid.Commutative.Monus
  , module Haskerwaul.Monoid.Frobenius
  , module Haskerwaul.Monoid.Frobenius.Commutative
  , module Haskerwaul.Monoid.Frobenius.Commutative.Special
  , module Haskerwaul.Monoid.Frobenius.Special
  , module Haskerwaul.Monoid.Hopf
  -- * objects
  , module Haskerwaul.Object
  , module Haskerwaul.Object.Free
  , module Haskerwaul.Object.Initial
  , module Haskerwaul.Object.NaturalNumbers
  , module Haskerwaul.Object.Terminal
  -- * orders
  , module Haskerwaul.Order.Canonical
  , module Haskerwaul.Order.Partial
  , module Haskerwaul.Order.Prefix
  , module Haskerwaul.Order.Total
  , module Haskerwaul.Preorder
  -- * pullbacks
  , module Haskerwaul.Pullback
  , module Haskerwaul.Pushout
  -- * quasigroups
  , module Haskerwaul.Quasigroup
  , module Haskerwaul.Quasigroup.Left
  , module Haskerwaul.Quasigroup.Right
  -- * relations
  , module Haskerwaul.Negation
  , module Haskerwaul.Relation.Apartness
  , module Haskerwaul.Relation.Apartness.Tight
  , module Haskerwaul.Relation.Binary
  , module Haskerwaul.Relation.Dependency
  , module Haskerwaul.Relation.Equality
  , module Haskerwaul.Relation.Equality.Decidable
  , module Haskerwaul.Relation.Equality.Stable
  , module Haskerwaul.Relation.Equivalence
  , module Haskerwaul.Relation.Equivalence.Partial
  , module Haskerwaul.Relation.Homogeneous
  , module Haskerwaul.Relation.Inequality
  , module Haskerwaul.Relation.Inequality.Denial
  , module Haskerwaul.Relation.Inequality.Tight
  , module Haskerwaul.Relation.Nullary
  , module Haskerwaul.Relation.Tolerance
  , module Haskerwaul.Relation.Unary
  -- * rigs
  , module Haskerwaul.Rig
  , module Haskerwaul.Rig.Idempotent
  , module Haskerwaul.Rig.Monus
  -- * rings
  , module Haskerwaul.Ring
  , module Haskerwaul.Ring.Commutative
  , module Haskerwaul.Ring.Nonunital
  -- * semigroups
  , module Haskerwaul.Semigroup
  , module Haskerwaul.Semigroup.Commutative
  , module Haskerwaul.Semigroup.Idempotent
  , module Haskerwaul.Semigroup.Inverse
  -- * semilattices
  , module Haskerwaul.Semilattice
  , module Haskerwaul.Semilattice.Bounded
  , module Haskerwaul.Semigroupoid
  -- * semirings
  , module Haskerwaul.Semiring
  , module Haskerwaul.Semiring.Idempotent
  , module Haskerwaul.Semiring.MaxPlus
  , module Haskerwaul.Semiring.MinPlus
  , module Haskerwaul.Semiring.Near
  , module Haskerwaul.Semiring.Pre
  , module Haskerwaul.Semiring.Pre.Near
  , module Haskerwaul.Semiring.Tropical
  -- * spans
  , module Haskerwaul.Cospan
  , module Haskerwaul.Span
  -- * subcategories
  , module Haskerwaul.Subcategory.Full
  -- * topoi (toposes)
  , module Haskerwaul.Topos.Elementary
  , module Haskerwaul.Topos.Grothendieck
  -- * natural transformations
  , module Haskerwaul.Transformation.Natural
  ) where

-- FIXME: causes conflicting exports for `epsilion` with
--       "Haskerwaul.Functor.Monoidal.Strong".
-- import Haskerwaul.Adjunction
import Haskerwaul.Algebra.Boolean
import Haskerwaul.Algebra.Heyting
import Haskerwaul.Algebra.Heyting.Complete
import Haskerwaul.Automorphism
import Haskerwaul.Bifunctor
import Haskerwaul.Bimonoid
import Haskerwaul.Category
import Haskerwaul.Category.Bicartesian
import Haskerwaul.Category.Closed
import Haskerwaul.Category.Closed.Bicartesian
import Haskerwaul.Category.Closed.Cartesian
import Haskerwaul.Category.Closed.Compact
import Haskerwaul.Category.CoKleisli
import Haskerwaul.Category.Concrete
import Haskerwaul.Category.Distributive
import Haskerwaul.Category.Duoidal
import Haskerwaul.Category.Duoidal.Normal
import Haskerwaul.Category.Hypergraph
import Haskerwaul.Category.Kleisli
import Haskerwaul.Category.Monoidal
import Haskerwaul.Category.Monoidal.Braided
import Haskerwaul.Category.Monoidal.Cartesian
import Haskerwaul.Category.Monoidal.Closed
import Haskerwaul.Category.Monoidal.Cocartesian
import Haskerwaul.Category.Monoidal.Distributive
import Haskerwaul.Category.Monoidal.Symmetric
import Haskerwaul.Category.Monoidal'
import Haskerwaul.Category.Opposite
import Haskerwaul.Category.Pointed
import Haskerwaul.Category.Product
import Haskerwaul.Category.Rig
import Haskerwaul.Category.Semigroupal
import Haskerwaul.Category.Terminal
import Haskerwaul.Cocone
import Haskerwaul.Cone
import Haskerwaul.Comonad
import Haskerwaul.Comonoid
import Haskerwaul.Cospan
import Haskerwaul.Day
import Haskerwaul.Dioid
import Haskerwaul.Duoid
import Haskerwaul.Duoid.Normal
import Haskerwaul.Embedding.Yoneda
import Haskerwaul.Endofunctor
import Haskerwaul.Extension.Kan.Left
import Haskerwaul.Extension.Kan.Right
import Haskerwaul.Field
import Haskerwaul.Field.Totalized
import Haskerwaul.Field.Totalized.One
import Haskerwaul.Field.Totalized.Zero
import Haskerwaul.Functor
import Haskerwaul.Functor.Closed.Cartesian
import Haskerwaul.Functor.Closed.Lax
import Haskerwaul.Functor.Closed.Strong
import Haskerwaul.Functor.Contravariant
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
import Haskerwaul.Law.Flexibility
import Haskerwaul.Law.Homomorphism.Identity
import Haskerwaul.Law.Homomorphism.Magma
import Haskerwaul.Law.Idempotency
import Haskerwaul.Law.Identity.Left
import Haskerwaul.Law.Identity.Right
import Haskerwaul.Law.Modularity
import Haskerwaul.Law.Quotient.Left
import Haskerwaul.Law.Quotient.Right
import Haskerwaul.Law.Reflexivity
import Haskerwaul.Law.Symmetry
import Haskerwaul.Loop
import Haskerwaul.Magma
import Haskerwaul.Magma.Commutative
import Haskerwaul.Magma.Idempotent
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
import Haskerwaul.Monoid.Hopf
import Haskerwaul.Negation
import Haskerwaul.Object
import Haskerwaul.Object.Free
import Haskerwaul.Object.Initial
import Haskerwaul.Object.NaturalNumbers
import Haskerwaul.Object.Terminal
import Haskerwaul.Order.Canonical
import Haskerwaul.Order.Partial
import Haskerwaul.Order.Prefix
import Haskerwaul.Order.Total
import Haskerwaul.Preorder
import Haskerwaul.Profunctor
import Haskerwaul.Pullback
import Haskerwaul.Pushout
import Haskerwaul.Quasigroup
import Haskerwaul.Quasigroup.Left
import Haskerwaul.Quasigroup.Right
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
import Haskerwaul.Rig
import Haskerwaul.Rig.Idempotent
import Haskerwaul.Rig.Monus
import Haskerwaul.Ring
import Haskerwaul.Ring.Commutative
import Haskerwaul.Ring.Nonunital
import Haskerwaul.Semigroup
import Haskerwaul.Semigroup.Commutative
import Haskerwaul.Semigroup.Idempotent
import Haskerwaul.Semigroup.Inverse
import Haskerwaul.Semilattice
import Haskerwaul.Semilattice.Bounded
import Haskerwaul.Semigroupoid
import Haskerwaul.Semiring
import Haskerwaul.Semiring.Idempotent
import Haskerwaul.Semiring.MaxPlus
import Haskerwaul.Semiring.MinPlus
import Haskerwaul.Semiring.Near
import Haskerwaul.Semiring.Pre
import Haskerwaul.Semiring.Pre.Near
import Haskerwaul.Semiring.Tropical
import Haskerwaul.Skewfield
import Haskerwaul.Span
import Haskerwaul.Subcategory.Full
import Haskerwaul.Topos.Elementary
import Haskerwaul.Topos.Grothendieck
import Haskerwaul.Transformation.Natural
