# Haskerwaul

[![Packaging status](https://repology.org/badge/tiny-repos/haskell:haskerwaul.svg)](https://repology.org/project/haskell:haskerwaul/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:haskerwaul.svg)](https://repology.org/project/haskell:haskerwaul/versions)

Category-parametric programming

## overview

This library gives an extremely abstract presentation of various CT and abstract algebra concepts.

The structure is largely based on [nLab](https://ncatlab.org/nlab/show/HomePage), with some additional references to [Wikipedia](https://www.wikipedia.org/). I've tried to link to as many pages as possible throughout the library to make it easy to understand all the concepts (or, at least as easy as getting your head around category theory is likely to be).

## usage

This library attempts to play well with the existing type classes in base. For example, we promote instances from base with instances like this:

```haskell
instance
  {-# OVERLAPPABLE #-}
  (Data.Functor.Functor f) =>
  Haskerwaul.Functor.Functor (->) (->) f
  where
  map = Data.Functor.fmap
```

Which also means that if you're defining your own instances, you'd be well-served to implement them using the type classes from base whenever possible, getting a bunch of Haskerwaul instances for free (for example, if you implement `Control.Category.Category`, I think you must get at least three separate Haskerwaul instances ([`Magma`](./src/Haskerwaul/Magma.hs), [`Semigroup`](./src/Haskerwaul/Semigroup.hs), and [`UnitalMagma`](./src/Haskerwaul/Magma/Unital.hs) -- things like [`Semicategory`](./src/Haskerwaul/Semicategory.hs) and [`Category`](./src/Haskerwaul/Category.hs) are either type synonyms or have universal instances already defined. Once you have the instance from base, you can implement richer classes like `CartesianClosedCategory` using Haskerwaul's classes.

However, this library doesn’t play well with `Prelude`, co-opting a bunch of the same names, so it's helpful to either enable `NoImplicitPrelude` or import Haskerwaul qualified. Unfortunately, [the `Haskerwaul` module](./src/Haskerwaul.hs) isn’t quite a custom Prelude and I've avoided expanding it into one, because there are a lot of points of contention when designing a Prelude. But perhaps it can be used as the basis of one.

### varieties of categories

- **Set**/**Hask** -- `(->)`
- `Constraint` -- `(:-)`
- [`Functor`](./src/Haskerwaul/Functor.hs) categories (including `Constraint`-
  valued functors
- [`Bifunctor`](./src/Haskerwaul/Bifunctor.hs) categories (separate from functor
  categories because we don't yet have real product categories)
- [`Opposite`](./src/Haskerwaul/Category/Opposite.hs) categories
- [`FullSubcategory`s](./src/Haskerwaul/Subcategory/Full.hs) by adding arbitrary
  constraints, filtering the objects in the category
- [`Kleisli`](./src/Haskerwaul/Category/Kleisli.hs) (and
  [`CoKleisli`](./src/Haskerwaul/Category/CoKleisli.hs)) categories over
  arbitrary categories
- [`ClosedCategory`s](./src/Haskerwaul/Category/Closed.hs) (beyond **Set**, and
  even beyond ones that have objects of kind `Type`)

## naming conventions

### types and classes

The names we use generally come from [nLab](https://ncatlab.org/nlab/show/HomePage), and I've tried to include links into nLab as much as possible in the Haddock.

### type parameters

There are a lot of one-character type parameters here, but we try to use them at least somewhat consistently.

- `ok` – _kinds_ of the objects in a category
- `ob` – _constraints_ on the objects in a category
- `c`, `c'`, `d` – categories representing the arrows of the category (kind `ok -> ok -> Type`)
- `a`, `b`, `x`, `y`, `z` – objects in a category and/or elements of those objects (kind `ok`)
- `t`, `t'`, `ct`, `dt` – tensors (kind `ok -> ok -> ok`) in a category (`ct` and `dt` distinguish when we're talking about tensors in categories `c` and `d`)

## [horizontal categorification](https://ncatlab.org/nlab/show/horizontal+categorification) / oidification

Horizontally categorified concepts are generally defined via type synonyms.
For example, the most central concept in this library is `Category`, which is defined
simply as

```haskell
type Category = HorizontalCategorification Monoid
```

See [the
examples](https://ncatlab.org/nlab/show/horizontal+categorification#examples)
for a more comprehensive list.

In cases where we can't simply use type synonyms, the Haddock should describe
why.

## law checking

Haskerwaul attempts to make it as easy and flexible as possible to test laws
using whatever mechanism you already use in your projects. We do this by
breaking things into relatively small pieces.

- Individual laws are defined under `Haskerwaul.Law` and should be minimally
  constrained, taking the required morphisms as arguments. These build a `Law`
  structure, which is basically just a tuple of morphisms that should commute.
- Aggregate laws for a structure are defined in
  `Haskerwaul.<structure>.Laws`. These should accumulate all the laws for their
  substructures as well.

The former are defined as abstractly as possible (for example, you can often define a
law for an arbitrary `MonoidalCategory`), while the latter currently require at
least an `ElementaryTopos` to get the characteristic morphism. However,
as you can see in `Haskerwaul.Semigroup.Laws` the laws can still be checked
against tensors other than the Cartesian product.

To test these laws, you need an `ElementaryTopos` for your testing
library. There is one for [Hedgehog](https://hedgehog.qa/) included. This then
lets you map each structure's laws into your topos, so you can test your own
instances. Since the structure laws are cumulative, this means you don't need
tests for `Magma _ _ Foo`, `UnitalMagma _ _ Foo`, etc. You should be able to do
a single test of `Group _ _ Foo`.

**NB**: One shortcoming is that since the structures are cumulative, and there
are often shared sub-structures in the tree, we currently end up testing the
more basic laws multiple times for a richer structure. It would be good to build
up the laws without duplication.

## what to work on

There are various patterns that indicate something can be improved.

### `__TODO__` or `__FIXME__`

These are the usual labels in comments to indicate that there is more work to be done on something. (The double-underscore on either side indicates to [Haddock](https://www.haskell.org/haddock/) that it should be bold when included in documentation.)

### `~ (->)`, `~ (,)`, `~ All`

Patterns like these are used when an instance has to be over-constrained. This
way we still get to use the type parameters we want in the instance declaration
and also have a way to `grep` for cases that are overconstrained, while arriving
at something that works in the mean time.

### newtypes

Newtypes are commonly used to workaround the "uniqueness" of instances. However,
aside from the other issues that we won't get into here, the fact that newtypes
are restricted to **Hask** means that we’re often overconstrained as a
result (see `~ (->)` above).

So, we try to avoid newtypes as much as possible. In `base`, you see things like

```haskell
newtype Ap f a = Ap { getAp :: f a }
instance (Applicative f, Monoid a) => Monoid (Ap f a) where
  ...
```

but that forces `f :: k -> Type` (granted, `Applicative` already forces
`f :: Type -> Type`, so it's no loss in `base`). However, we want to stay more
kind-polymorphic, so we take a different tradeoff and write stuff like

```haskell
instance (LaxMonoidalFunctor c ct d dt f, Monoid c ct a) => Monoid d dt (f a)
```

(where `LaxMonoidalFunctor` is our equivalent of `Applicative`), which means we
need `UndecidableInstances`, but it's worth it to be kind-polymorphic.

### relations

The way relations are currently implemented means that an `EqualityRelation`
_is_ a `PartialOrder`, and since these are defined with type classes, it means
the `PartialOrder` that’s richer than the discrete one implied by the
`EqualityRelation` needs to be made distinct via a newtype. And similarly, the
(unique) `EqualityRelation` also needs a newtype to make it distinct from the
`InequalityRelation` that it's derived from.
`nix build` will build and test the project fully.

See "newtypes" above.

### enriched categories

In "plain" category theory, the Hom functor for a category **C** is a functor **C** x **C** -> **Set**. Enriched category theory generalizes that **Set** to some arbitrary monoidal category **V**. For example, in the case of preorders, **V** may be **Set** (where the image is only singleton sets and the empty set) or it can be **Bool**, which more precisely models the exists-or-not nature of the relationship.

One way to model this is to add a parameter `v` to `Category c`, like

```haskell
class Monoid (DinaturalTransformation v) Procompose c => Category v c
```

However, this means that the same category, enriched differently, has multiple instances. Modeling the enrichment separately, for example,

```haskell
class (MonoidalCategory v, Category c) => EnrichedCategory v c
```

seems good, but the Hom functor is fundamental to the definition of composition in `c`. Finally, perhaps we can encode some "primary" **V** existentially, and model other enrichments via functors from **V**.

### PolyKinds

This library strives to take advantage of `PolyKinds`, which make it possible to say things like `Category ~ Monoid (NaturalTransformation (->)) Procompose`, however we use newtypes like `Additive` and `Multiplicative` to say things like `Semiring a ~ (Monoid (Additive a), Monoid (Multiplicative a))`, and since fully-applied type constructors need to have kind `Type` it means that `Semiring` _isn't_ kind-polymorphic.

As a result, at various places in the code we find ourselves stuck dealing with `Type` when we'd like to remain polymorphic. Remedying this would be helpful.

## licensing

This package is licensed under [The GNU AGPL 3.0 or later](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20no-recursion).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

## versioning

This project largely follows the [Haskell Package Versioning Policy](https://pvp.haskell.org/) (PVP), but is more strict in some ways.

The version always has four components, `A.B.C.D`. The first three correspond to those required by PVP, while the fourth matches the “patch” component from [Semantic Versioning](https://semver.org/).

Here is a breakdown of some of the constraints:

### sensitivity to additions to the API

PVP recommends that clients follow [these import guidelines](https://wiki.haskell.org/Import_modules_properly) in order that they may be considered insensitive to additions to the API. However, this isn’t sufficient. We expect clients to follow these additional recommendations for API insensitivity

If you don’t follow these recommendations (in addition to the ones made by PVP), you should ensure your dependencies don’t allow a range of `C` values. That is, your dependencies should look like

```cabal
yaya >=1.2.3 && <1.2.4
```

rather than

```cabal
yaya >=1.2.3 && <1.3
```

#### use package-qualified imports everywhere

If your imports are [package-qualified](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/package_qualified_imports.html?highlight=packageimports#extension-PackageImports), then a dependency adding new modules can’t cause a conflict with modules you already import.

#### avoid orphans

Because of the transitivity of instances, orphans make you sensitive to your dependencies’ instances. If you have an orphan instance, you are sensitive to the APIs of the packages that define the class and the types of the instance.

One way to minimize this sensitivity is to have a separate package (or packages) dedicated to any orphans you have. Those packages can be sensitive to their dependencies’ APIs, while the primary package remains insensitive, relying on the tighter ranges of the orphan packages to constrain the solver.

### transitively breaking changes (increments `A`)

#### removing a type class instance

Type class instances are imported transitively, and thus changing them can impact packages that only have your package as a transitive dependency.

#### widening a dependency range with new major versions

This is a consequence of instances being transitively imported. A new major version of a dependency can remove instances, and that can break downstream clients that unwittingly depended on those instances.

A library _may_ declare that it always bumps the `A` component when it removes an instance (as this policy dictates). In that case, only `A` widenings need to induce `A` bumps. `B` widenings can be `D` bumps like other widenings, Alternatively, one may compare the APIs when widening a dependency range, and if no instances have been removed, make it a `D` bump.

### breaking changes (increments `B`)

#### restricting an existing dependency’s version range in any way

Consumers have to contend not only with our version bounds, but also with those of other libraries. It’s possible that some dependency overlapped in a very narrow way, and even just restricting a particular patch version of a dependency could make it impossible to find a dependency solution.

#### restricting the license in any way

Making a license more restrictive may prevent clients from being able to continue using the package.

#### adding a dependency

A new dependency may make it impossible to find a solution in the face of other packages dependency ranges.

#### any change to fixity

There is no change to fixity that can ensure it doesn’t break existing parses.

### non-breaking changes (increments `C`)

#### adding a module

This is also what PVP recommends. However, unlike in PVP, this is because we recommend that package-qualified imports be used on all imports.

### other changes (increments `D`)

#### widening a dependency range for non-major versions

This is fairly uncommon, in the face of `^>=`-style ranges, but it can happen in a few situations.

#### deprecation

**NB**: This case is _weaker_ than PVP, which indicates that packages should bump their major version when adding `deprecation` pragmas.

We disagree with this because packages shouldn’t be _publishing_ with `-Werror`. The intent of deprecation is to indicate that some API _will_ change. To make that signal a major change itself defeats the purpose. You want people to start seeing that warning as soon as possible. The major change occurs when you actually remove the old API.

Yes, in development, `-Werror` is often (and should be) used. However, that just helps developers be aware of deprecations more immediately. They can always add `-Wwarn=deprecation` in some scope if they need to avoid updating it for the time being.

## licensing

This package is licensed under [The GNU AGPL 3.0 or later](./LICENSE). If you need a license for usage that isn’t covered under the AGPL, please contact [Greg Pfeil](mailto:greg@technomadic.org?subject=licensing%20haskerwaul).

You should review the [license report](docs/license-report.md) for details about dependency licenses.

## comparisons

There are a number of libraries that attempt to encode categories in various ways. I try to explain how Haskerwaul compares to each of them, to make it easier to decide what you want to use for your own project. I would be very happy to get additional input for this section from people who have used these other libraries (even if it's just to ask a question, like "how does Haskerwaul's approach to X compare to SubHask, which does Y?").

In general, Haskerwaul is much more fine-grained than other libraries. It also has many small modules with minimal dependencies between them. Haskerwaul is also not quite as ergonomic as other libraries in many situations. For example, `Magma`'s `op` is the least meaningful name possible and it occurs _everywhere_ in polymorphic functions, meaning anything from function composition to addition, meet, join, etc. And there are currently plenty of places where (thanks to `newtype`s) the category ends up constrained to `->`, which is a frustrating limitation.

### [algebra](http://hackage.haskell.org/package/algebra)

### [categories](http://hackage.haskell.org/package/categories)

This adds a handful of the concepts defined in Haskerwaul, and those it includes are designed to be compatible with the existing `Category` type class in `base`. For example, the categories aren’t constrained. Haskerwaul's `Category` is generalized and fits into a much larger framework of categorical concepts.

### [concat](https://github.com/conal/concat)

The primary component of this is a compiler plugin for category-based rewriting. However, it needs a category hierarchy to perform the rewrites on, so it provides one of its own. The hierarchy is pretty small, restricts objects to kind `Type`, also has a single definition for products, coproducts, exponentials, etc., which reduces the flexibility a lot. Finally, concat has some naming conventions (and hierarchy) that perhaps better serves Haskell programmers understanding the mechanism than modeling categorical concepts. That is, it uses names like `NumCat`, which is a `Category`-polymorphic `Num` class rather than a name like `Ring` that ties it more to category theory (or at least abstract algebra).

### [constrained-categories](https://hackage.haskell.org/package/constrained-categories)

### [HaskellForMaths](http://hackage.haskell.org/package/HaskellForMaths)

### [SubHask](http://hackage.haskell.org/package/subhask)

SubHask uses a similar mechanism for subcategories, an associated type family (`ValidCategory` as opposed to `Ob`) to add constraints to operations. But it doesn't generalize across kinds (for example, a `Category` isn't a `Monoid`). It also doesn't allow categories to be monoidal in multiple ways, as the tensor is existential.
