# Haskerwaul

[![built with garnix](https://img.shields.io/endpoint?url=https%3A%2F%2Fgarnix.io%2Fapi%2Fbadges%2Fsellout%2Fhaskerwaul)](https://garnix.io)
[![Packaging status](https://repology.org/badge/tiny-repos/haskell:haskerwaul.svg)](https://repology.org/project/haskell:haskerwaul/versions)
[![latest packaged versions](https://repology.org/badge/latest-versions/haskell:haskerwaul.svg)](https://repology.org/project/haskell:haskerwaul/versions)

Howling into the primordial ooze of category theory.

## overview

This library gives an extremely abstract presentation of various CT and abstract algebra concepts.

The structure is largely based on [nLab](https://ncatlab.org/nlab/show/HomePage), with some additional references to [Wikipedia](https://www.wikipedia.org/). I've tried to link to as many pages as possible throughout the library to make it easy to understand all the concepts (or, at least as easy as getting your head around category theory is likely to be).

## usage

This library attempts to play well with the existing type classes in base. E.g., we promote instances from base with instances like this:

```haskell
instance
  {-# OVERLAPPABLE #-}
  (Data.Functor.Functor f) =>
  Haskerwaul.Functor.Functor (->) (->) f
  where
  map = Data.Functor.fmap
```

Which also means that if you're defining your own instances, you'd be well-served to implement them using the type classes from base whenever possible, getting a bunch of Haskerwaul instances for free (e.g., if you implement `Control.Category.Category`, I think you must get at least three separate Haskerwaul instances ([`Magma`](./src/Haskerwaul/Magma.hs), [`Semigroup`](./src/Haskerwaul/Semigroup.hs), and [`UnitalMagma`](./src/Haskerwaul/Magma/Unital.hs) -- things like [`Semicategory`](./src/Haskerwaul/Semicategory.hs) and [`Category`](./src/Haskerwaul/Category.hs) are either type synonyms or have universal instances already defined. Once you have the instance from base, you can implement richer classes like `CartesianClosedCategory` using Haskerwaul's classes.

However, this library does not play well with `Prelude`, co-opting a bunch of the same names, so it's helpful to either enable `NoImplicitPrelude` or import Haskerwaul qualified. Unfortunately, [the `Haskerwaul` module](./src/Haskerwaul.hs) is not quite a custom Prelude and I've avoided expanding it into one, because there are a lot of points of contention when designing a Prelude. But perhaps it can be used as the basis of one.

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

- `ok` -- _kinds_ of the objects in a category
- `ob` -- _constraints_ on the objects in a category
- `c`, `c'`, `d`, ... -- categories representing the arrows of the category (kind `ok -> ok -> Type`)
- `a`, `b`, `x`, `y`, `z` -- objects in a category and/or elements of those objects (kind `ok`)
- `t`, `t'`, `ct`, `dt` -- tensors (kind `ok -> ok -> ok`) in a category (`ct` and `dt` distinguish when we're talking about tensors in categories `c` and `d`)

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

The former are defined as abstractly as possible (e.g., you can often define a
law for an arbitrary `MonoidalCategory`), while the latter currently require at
least an `ElementaryTopos` in order to get the characteristic morphism. However,
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
are restricted to **Hask** means that we are often very overconstrained as a
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
the `PartialOrder` that is richer than the discrete one implied by the
`EqualityRelation` needs to be made distinct via a newtype. And similarly, the
(unique) `EqualityRelation` also needs a newtype to make it distinct from the
`InequalityRelation` that it's derived from.
`nix build` will build and test the project fully.

See "newtypes" above.

### enriched categories

In "plain" category theory, the Hom functor for a category **C** is a functor **C** x **C** -> **Set**. Enriched category theory generalizes that **Set** to some arbitrary monoidal category **V**. E.g., in the case of preorders, **V** may be **Set** (where the image is only singleton sets and the empty set) or it can be **Bool**, which more precisely models the exists-or-not nature of the relationship.

One way to model this is to add a parameter `v` to `Category c`, like

```haskell
class Monoid (DinaturalTransformation v) Procompose c => Category v c
```

However, this means that the same category, enriched differently, has multiple instances. Modeling the enrichment separaetely, e.g.,

```haskell
class (MonoidalCategory v, Category c) => EnrichedCategory v c
```

seems good, but the Hom functor is fundamental to the definition of composition in `c`. Finally, perhaps we can encode some "primary" **V** existentially, and model other enrichments via functors from **V**.

### PolyKinds

This library strives to take advantage of `PolyKinds`, which make it possible to say things like `Category ~ Monoid (NaturalTransformation (->)) Procompose`, however we use newtypes like `Additive` and `Multiplicative` to say things like `Semiring a ~ (Monoid (Additive a), Monoid (Multiplicative a))`, and since fully-applied type constructors need to have kind `Type` it means that `Semiring` _isn't_ kind-polymorphic.

As a result, at various places in the code we find ourselves stuck dealing with `Type` when we'd like to remain polymorphic. Remedying this would be very helpful.

## development environment

We recommend the following steps to make working in this repository as easy as possible.

### `direnv allow`

This command ensures that any work you do within this repository is done within a consistent reproducible environment. That environment provides various debugging tools, etc. When you leave this directory, you will leave that environment behind, so it doesn’t impact anything else on your system.

### `git config --local include.path ../.config/git/config`

This will apply our repository-specific Git configuration to `git` commands run against this repository. It’s lightweight (you should definitely look at it before applying this command) – it does things like telling `git blame` to ignore formatting-only commits.

## building & development

Especially if you are unfamiliar with the haskell ecosystem, there is a Nix build (both with and without a flake). If you are unfamiliar with Nix, [Nix adjacent](...) can help you get things working in the shortest time and least effort possible.

### if you have `nix` installed

`nix build` will build and test the project fully.

`nix develop` will put you into an environment where the traditional build tooling works. If you also have `direnv` installed, then you should automatically be in that environment when you're in a directory in this project.

### traditional build

This project is built with [Cabal](https://cabal.readthedocs.io/en/stable/index.html). Individual packages will work with older versions, but ./cabal.package requires Cabal 3.6+.

## versioning

In the absolute, almost every change is a breaking change. This section describes how we mitigate that to provide minor updates and revisions.

Here are some common changes that can have unintended effects:

- adding instances can conflict with downstream orphans,
- adding a module can conflict with a module from another package,
- adding a definition to an existing module can conflict if there are unqualified imports, and
- even small bugfixes can introduce breaking changes where downstream depended on the broken results.

To mitigate some of those issues for versioning, we assume the following usage:

- modules should be imported using `PackageImports`, so that adding modules is a _minor_ change;
- modules should be imported qualified, so that adding definitions is a _minor_ change;
- adding instances can't be mitigated in the same way, and it's not uncommon for downstream libraries to add orphans instances when they're omitted from upstream libraries. However, since these conflicts can only happen via direct dependencies, and represent an explicit downstream workaround, it’s reasonable to expect a quick downstream update to remove or conditionalize the workaround. So, this is considered a _minor major_ change;
- deprecation is considered a _revision_ change, however it will often be paired with _minor_ changes. `-Werror` can cause this to fail, but published libraries shouldn't be compiled with `-Werror`.

## comparisons

There are a number of libraries that attempt to encode categories in various ways. I try to explain how Haskerwaul compares to each of them, to make it easier to decide what you want to use for your own project. I would be very happy to get additional input for this section from people who have used these other libraries (even if it's just to ask a question, like "how does Haskerwaul's approach to X compare to SubHask, which does Y?").

In general, Haskerwaul is much more fine-grained than other libraries. It also has many small modules with minimal dependencies between them. Haskerwaul is also not quite as ergonomic as other libraries in many situations. E.g., `Magma`'s `op` is the least meaninful name possible and it occurs _everywhere_ in polymorphic functions, meaning anything from function composition to addition, meet, join, etc. And there are currently plenty of places where (thanks to `newtype`s) the category ends up constrained to `->`, which is a very frustrating limitation.

### [algebra](http://hackage.haskell.org/package/algebra)

### [categories](http://hackage.haskell.org/package/categories)

This adds a handful of the concepts defined in Haskerwaul, and those it includes are designed to be compatible with the existing `Category` type class in `base`. E.g., the categories are not constrained. Haskerwaul's `Category` is generalized and fits into a much larger framework of categorical concepts.

### [concat](https://github.com/conal/concat)

The primary component of this is a compiler plugin for category-based rewriting. However, it needs a category hierarchy to perform the rewrites on, so it provides one of its own. The hierarchy is pretty small, restricts objects to kind `Type`, also has a single definition for products, coproducts, exponentials, etc., which reduces the flexibility a lot. Finally, concat has some naming conventions (and hierarchy) that perhaps better serves Haskell programmers understanding the mechanism than modeling categorical concepts. I.e., it uses names like `NumCat`, which is a `Category`-polymorphic `Num` class rather than a name like `Ring` that ties it more to category theory (or at least abstract algebra).

### [constrained-categories](https://hackage.haskell.org/package/constrained-categories)

### [HaskellForMaths](http://hackage.haskell.org/package/HaskellForMaths)

### [SubHask](http://hackage.haskell.org/package/subhask)

SubHask uses a similar mechanism for subcategories, an associated type family (`ValidCategory` as opposed to `Ob`) to add constraints to operations. But it doesn't generalize across kinds (e.g., a `Category` isn't a `Monoid`). It also doesn't allow categories to be monoidal in multiple ways, as the tensor is existential.
