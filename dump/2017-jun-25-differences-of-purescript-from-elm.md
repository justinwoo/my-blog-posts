---
title: Differences of Purescript from Elm
tags: purescript Elm
author: kimagure
slide: false
---
**This article is fairly old, so you might look at my resources guide here: <https://purescript-resources.readthedocs.io/en/latest/>**

*(The resource that I wish I had when starting with Purescript, or when revisiting Elm)*

Gist version here: https://gist.github.com/justinwoo/0118be3e4a0d7394a99debbde2515f9b
 
The syntax between Elm and Purescript are mostly the same, to the point where most Elm code can be converted to Purescript with copy-pasting and find-replace/multi-cursor editing.

## Purpose of this document

Many people who have used Elm who are curious about/have tried/are using Purescript have asked me to put together a "differences from Elm" post to help them, and which I would also have found helpful.

## Type Annotations

Type annotations are written using `::` (as `:` is used for constructing list nodes).

```hs
a :: Int
a = 1
```

## Type Aliases

The `type` keyword itself is used to define type aliases. You should be careful to use meaningful type aliases when writing your programs.

```hs
type Url = String -- not recommended, as any 'Url' can be fed a String

type Result a = Either Error a
```

The way "type" is used in Elm is used with data type declarations. There are two such cases:

### Data

As explained below, this is most often used when you have various sums and products you need to express, like so:

```hs
data Fruit
  = Apple
  | Banana
  | Frikandel

data Coords = Coords Int Int
```

### Newtype

This is a special type of data type that has a single argument. These are especially useful when dealing with raw data, where you can write a validation function and not expose the constructor in exports.

```hs
module Url
  ( Url
  , validateUrl
  ) where

newtype Url = ValidatedUrl String

validateUrl :: String -> Either Error ValidatedUrl
validateUrl = ???
```

There are many useful methods available for working with newtypes using the [Newtype](https://github.com/purescript/purescript-newtype) package.

I have a video I've made about this on [Egghead](https://egghead.io/lessons/model-problems-using-algebraic-data-types-adts) that might be helpful to understand this.

## Comments

They are the same, but libraries will have pipe for comments exported to documentation:

```hs
-- hello

{-
world
-}

-- | documentation comment
```

## Literals

### Boolean

The type of booleans is `Boolean`, with `true` and `false` being the actual values used, which are represented as the actual Javascript booleans in the runtime.

```hs
true :: Boolean
false :: Boolean
```

### Numbers

Normally one uses `Int` for integers, and they are represented as any number literal. Javascript numbers are also available.

```hs
12 :: Int

123.123 :: Number -- note: this is a floating point JS number
```

### Strings

Strings can be represented in the same way as in Elm.

```hs
"hello"

"""
multiline
"""
```

## Lists

Because normal lists do not have a Javascript representation, arrays are used with square brackets in Purescript, and are represented in Javascript as arrays.

```hs
[1,2,3] :: Array -- an actual Javascript array
```

For normal lists, there is a [Lists](https://github.com/purescript/purescript-lists) package with both strict and lazy lists.

## Conditionals

These are the same as with Elm.

```hs
if condition
then one
else two
```

For many types, you may be interested in using the [Control](https://github.com/purescript/purescript-control) library, which provides methods such as `when`.

## Union Types

While most languages and literature call these "Algebraic Data Types (ADT) of sums and products", it is easier to know of them as the two:

### Sum Types

These may also be called [Tagged union](https://en.wikipedia.org/wiki/Tagged_union), variant, discriminated/disjoint union, or sum types. These represent a `A + B` relationship of type constructors of a data type. For example:

```hs
data Fruit -- Fruit is the data type
  = Apple  -- Apple is a type constructor
  | Banana -- Banana is a type constructor
```

### Product Types

These are most often known as a [Product type](https://en.wikipedia.org/wiki/Product_type). These represent a `A * B` relationship between a type constructor and its arguments.

```hs
data MyBurger = Burger Bread Patty -- Burger is the type constructor, with Bread and Patty being its arguments
```

These two are then put together to form data types you will want to work with:

```hs
data Breakfast
  = Toast
  | Cereal Milk
```

### A note about "Union Types"

[Union types](https://en.wikipedia.org/wiki/Union_type#Untagged_unions), usually known as untagged unions, are available in some languages and can be very useful. For example, [Typescript](https://www.typescriptlang.org/docs/handbook/advanced-types.html#union-types) comes with union types to provide you ways of working with various types that can be given to a function, as with Flowtype. You may soon find that you almost never need these though, as Elm has likely already shown.

## Records

Records in Purescript are also similar to Elm in usage.

```hs
a = { x: 3, y: 4}

type MyRecordAlias =
  { x :: Int
  , y :: Int
  }
```

## Functions

These are the same as in Elm, but also allow for matching on patterns and using guards. Exhaustivity of patterns is checked by the compiler.

```hs
addTwo x = x + 2

go (Finished x) = x
go (Continue x) = -- ...

greaterThanTwo x
  | x > 2 = true
  | otherwise = false
```

## Infix Operators

### Definition

These are largely the same as with Elm, but operators are defined only as aliases for normal functions. There is also no restriction on what operators may be defined, as all Purescript modules (including the normal Prelude) are normal libraries.

```hs
applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped x f = f x

infixl 1 applyFlipped as #
```

### Infix functions

Functions can also be infixed in Purescript and can be useful.

```hs
Pattern "ppl" `indexOf` "Apple"
```

### Function Application/Composition

In Elm, function application is largely done using a left-to-right "pipe" operator. Many users prefer to use right-to-left operators, but any are suitable for use.

** As Qiita's markdown parser does not correctly parse escapes, I cannot show the right pipe `|>` or left pipe `<|` correctly here.

Elm | Purescript
--- | ---
right pipe | `#`
left pipe | `$`
`>>` | `>>>`
`<<` | `<<<`

## Let expressions

The same expressions are in Purescript, but many users will prefer using `where` instead to put the binding at the end, or use `do` notation with `let` bindings.

```hs
fn1 =
  let
    a = 2
  in
    a + 2

fn2 =
  a + 2
  where
    a = 2

fn3 = do
  let a = 2
  a + 2
```

## Tuples

Tuples are available through the [Tuple](https://github.com/purescript/purescript-tuples) library, as they do not have a Javascript representation and do not require a special library.

```hs
a = Tuple 1 2 -- using purescript-tuples
```

## Modules

Modules are mostly similar but look a bit different:

```hs
module MyModule
  ( exported
  ) where

import Prelude -- only one top-level import is recommended
import Data.Array as A
import Data.List (head)
import Data.Map hiding (singleton)
```

## Finding what you need

[Pursuit](https://pursuit.purescript.org/) and [Type holes with Type-Directed Search](https://github.com/paf31/24-days-of-purescript-2016/blob/master/23.markdown) will be your best friends in finding what you need when writing Purescript, and will provide support that you don't normally see in many languages.

### Pursuit

[Pursuit](https://pursuit.purescript.org/) is a search engine for published packages in the Purescript ecosystem, and provides search-by-name and search-by-type-signature. For example, you can search for [encodeURI](https://pursuit.purescript.org/search?q=encodeURI), and you can also search for [`(a -> b) -> f a -> f b`](https://pursuit.purescript.org/search?q=%28a+-%3E+b%29+-%3E+f+a+-%3E+f+b).

### Type holes

In the right-hand side of a `=` for a definition, you can use type holes with any identifier prefixed with a question mark, like `?abc`. If a type matches this hole, a suggestion will be given by the compiler. For example:

```hs
a :: Array Int
a = ?whatGoesHere ((+) 1) [1,2,3]
```

Will give you a suggestion in this form:

```hs
  Hole 'whatGoesHere' has the inferred type

    (Int -> Int) -> Array Int -> Array Int

  You could substitute the hole with one of these values:

    Data.Functor.map            :: forall a b f. Functor f => (a -> b) -> f a -> f b
```

### Editor support

As the IDE server for Purescript ships with the compiler, there is no need to worry about having your own IDE tools be out of date. There are many editor integrations for this server:

* [Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=nwolverson.ide-purescript)
* [Atom](https://atom.io/packages/ide-purescript)
* [Vim](https://github.com/FrigoEU/psc-ide-vim)
* [Emacs](https://github.com/epost/psc-ide-emacs)
* [Spacemacs](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/purescript)

## Type classes

Type classes are a way to define methods that may be defined for a given type, which can be instantiated to the type. In this way, the types are like objects of a class, and can be found statically.

One example would be look at the Eq type class for defining equality of two elements of a type:

```hs
class Eq a where
  eq :: a -> a -> Boolean
```

You can define an instance for this class for your own data type and use the `eq` method readily:

```hs
data Coords = Coords Int Int
instance eqCoords :: Eq Coords where
  eq (Coords x1 y1) (Coords x2 y2) = x1 == x2 && y1 == y2

a = eq (Coords 1 1) (Coords 1 1) -- true
b = eq (Coords 1 2) (Coords 3 4) -- false
```

Some of the basic type classes can be derived automatically, so writing that definition of `Eq Coords` was actually unnecessary! Read more about this in [this post](https://github.com/paf31/24-days-of-purescript-2016/blob/master/3.markdown#3-deriving-eq-and-ord).

You can read more about this in [Purescript by Example](https://leanpub.com/purescript/read#leanpub-auto-type-classes).

That said, you **don't** have to use many type classes to start writing Purescript. You can get writing by mostly exploiting type holes and Type-Directed Search for them as written [here](https://github.com/paf31/24-days-of-purescript-2016/blob/master/23.markdown).

## En/Decoding JSON

In Elm, you might be used to writing decoders and encoders yourself and using them where necessary. Purescript helps you resolve these methods statically via [type classes](https://leanpub.com/purescript/read#leanpub-auto-type-classes) in packages like [Foreign-Generic](https://github.com/paf31/purescript-foreign-generic/) and [Argonaut](https://github.com/purescript-contrib/purescript-argonaut).

The best part is that for most normally representable JSON structures, there is almost no work involved! See my [Howto-Foreign-Generic](https://github.com/justinwoo/purescript-howto-foreign-generic) demo, video, and slides to learn more.

## Where next?

You can quite readily start porting your existing Elm applications to [Pux](http://purescript-pux.org/) if you want to make a frontend web application. For a truly component-based approach, you might be interested in using [Halogen](https://github.com/slamdata/purescript-halogen).

[Purescript by Example](https://leanpub.com/purescript/read) is a complete guide to Purescript if you want to learn more about the language, and the community is available through a number of media through [FPChat.com Slack](https://fpchat.com), Freenode IRC #purescript, [Gitter](https://gitter.im/purescript/purescript), Twitter #purescript, Github issues, and others.

**You can also ping me on FPChat.com Slack in #purescript or on [Twitter](https://twitter.com/jusrin00) and I and many others will try to help you get things done.**

## Other FAQ's

* Why Bower?

Harry has written a post called ["Why the PureScript community uses Bower (or, perhaps more appropriately, 'Why the PureScript community does not use npm')"](http://harry.garrood.me/blog/purescript-why-bower/) that you should read on this topic.

Because of nested dependencies in npm/yarn, we cannot actually guarantee that the correct versions of packages are being used throughout the system, which will lead to compile-time errors when the wrong packages are inevitably required. If you are a Typescript user, you have probably also seen this in various forms such as `"type Observable<MyType> is not equal to type Observable<MyType>"`.

* Why do I have to read X from Haskell?

Inevitably, all packages in all communities, especially smaller ones like Purescript and Elm, suffer from not being completely fleshed out in documentation, technical manuals, etc. The benefit of Purescript is that because you are exposed to many of the common terminology and patterns effectively used in the Haskell world, you can readily use documentation and blog posts that have been written for similar Haskell libraries/features/techniques to Purescript ones.

You may even become a Haskell user over time, as was my case in starting to use Haskell *after* using Purescript.

As the joke goes, "Haskell is an incubator for Purescript ideas".

* Is there a comparison chart of different things?

Håkon has a project that catalogs of differences [here](https://hakonrossebo.github.io/functional-programming-babelfish/).

