---
title: What I've learned since quitting Elm
tags: purescript
author: kimagure
slide: false
---
In 2015, I spent some of my spare time trying out Elm, to render something simple in the browser and also to run some programs in Node. During those times, there were very few restrictions on what the main function could be, so writing a Node program was very simple, especially given the Stream type that allowed for programming in a simple reactive way. I quit using Elm after a while of being constantly frustrated with its alternative dictionary of terms, lack of links to external resources, more restriction to writing only the same kind of front end apps, false solutions (e.g. ["Just use scrap your typeclasses"](http://reasonablypolymorphic.com/blog/elm-is-wrong)), and the way changes and restrictions were communicated in terms of "thought leader knows best", long before the release of Elm 0.17, which killed the way I wrote applications using Streams.

In this post, I'll just highlight some of the most important things I've learned since the beginning of 2016 by using PureScript.

## Newtypes

I didn't know how I could get a named type that would be structurally the same as the type that it wraps:

```hs
newtype Name = Name String
```

This has proven to be incredibly useful for preventing mistakes and ensuring correct usage.

## Algebraic Data Types

When I worked with "Union types", I didn't actually know if these were at all like the untagged unions I'd been used to using from dynamic languages and gradually typed ones (e.g. https://docs.racket-lang.org/ts-guide/types.html).

I finally learned that data types have constructor(s) associated with them that take a number of arguments. A "union type" was a sum type, where the type is constructed as a sum of its constructors, and each constructor actually was either a value (when it has no arguments) or a function (when it takes one or more arguments):

```hs
data Fruit
  = Apple
  | Banana Int

mkApple :: Fruit
mkApple = Apple

mkBanana :: Int -> Fruit
mkBanana = Banana
```

## Kinds, or "types of types"

I learned that there is also the concept of Kinds, where the `Type` kind is just one of many. While `Type` is special in that it has values in runtime, there are many other kinds that are defined in the compiler and can be further defined in libraries. For example, records are defined by a row type with first a `# Type` argument which then gives me a concrete type that can have values:

```hs
type MyRow = ( a :: Int, b :: String )

-- from Prim: data Record :: # Type -> Type
type MyRecord = Record MyRow
type MyRecord2 = { | MyRow }
```

## "Type classes" is not a single feature

Before, I had been told that "type classes" were simply a way to work with implicitly provided functions based on a single type's parameter, such as `compare :: forall a. Ord a => a -> a -> Ordering`. In reality, we could have type classes with multiple parameters, and type classes don't necessarily even need a method associated with them:

```hs
class MyClass a b c
```

I also learned about Functional Dependencies, that I can write my instances to these classes in terms of only matching for some of the parameters, declaring that some of the type parameters could be used to match others:

```hs
class MyClass a b c | a -> b c
```

In fact, this is how `Row.Cons` works in PureScript:

```hs
class Cons (label :: Symbol) (a :: Type) (tail :: # Type) (row :: # Type)
 | label a tail -> row, label row -> a tail
```

Given a label and row type, we can get the type at that field and the complement row type that does not contain the field, which lets us implement a generic `Record.get` function:

```hs
get :: forall r r' l a. IsSymbol l => Cons l a r' r =>
  SProxy l -> { | r } -> a
```

With this, we're also able to construct a new row type, so we can also implement a `Record.set` function to change the type of a record field:

```hs
set :: forall r1 r2 r l a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 =>
  SProxy l -> b -> { | r1 } -> { | r2 }
```

This led me to realize that we really can use type information to derive routines, so we can derive JSON decoders from type information directly: <https://github.com/justinwoo/purescript-simple-json>

I also learned that "type level programming" largely revolves around thoughtful applications of this knowledge, not that it's just undecipherable magic.

## Datatype Generics

I always wondered how I could use data types generically, since I should already have enough information from defining my data types. Turns out, this is a thing, and the compiler can derive this for us.

In short, there is a type class `Generic`, which has the definition

```hs
class Generic a rep | a -> rep where
  to :: rep -> a
  from :: a -> rep
```

Then for a defined data type, we can derive `Generic`:

```hs
data Fruit
  = Abogado
  | Boat
  | Candy

derive instance genericFruit :: Generic Fruit _
```

And we can use generic implementations on types with an instance of `Generic`:

```hs
instance furitShow :: Show Fruit where
  show = genericShow
```

This all works because the `rep` above are just a series of types like the following:

```hs
-- | A representation for types with multiple constructors.
data Sum a b = Inl a | Inr b

-- | A representation for constructors which includes the data constructor name
-- | as a type-level string.
newtype Constructor (name :: Symbol) a = Constructor a

-- | A representation for an argument in a data constructor.
newtype Argument a = Argument a
```

Like in the following examples:

```hs
data Things = Apple Int |   Banana String
--            a         Sum b
-- e.g. Sum (Inl a) (Inr b)

data Things = Apple             Int | Banana String
--            Constructor(name) a
-- e.g. Constructor "Apple" a

data Things = Apple Int | Banana String
--                  Argument(a)
-- e.g. Argument Int
```

I wrote a tutorial for this in the Simple-JSON docs, so you can read more about datatype generics here <https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html>

## Conclusion

So there are also many other topics I didn't cover here, but I hope this has given you some ideas on what all you could start learning about and using to solve problems you run into.

I really only came to know about PureScript thanks to some efforts by Bodil to publicize it, who I knew from some various people associated with RxJS and Cycle.JS. I think even now, very few people who would actually really like to use PureScript know of it, so I'd encourage people to give some small intro talks at local meetups about it.

## P.S.

I guess people assume you have to know Haskell to use PureScript, but I started using Haskell after using PureScript for a couple of months. Since then, I've been writing some small amounts of Haskell to help maintain some projects.

## Links

* PureScript language reference <https://github.com/purescript/documentation/tree/master/language>
* My "opinionated" reference of PureScript <https://purescript-resources.readthedocs.io/en/latest/>

