# Differences of Purescript from Elm

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
