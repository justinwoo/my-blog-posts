---
title: Converting Generics-Rep to Row Types in PureScript
tags: purescript
author: kimagure
slide: false
---
Previously, I wrote about turning Generics-Rep types into a `RowList`-like structure in [Generics-Rep Sums and Products to List for Fun](https://qiita.com/kimagure/items/a5e340242f038b0dc748). The other day, I thought about how I would much rather use row types for this, but ran into the problem that while I had the constructor names available in sum type generics, product types would not be able to extract information out in a useful way since there is no proper way to get the name of a type in PureScript.

I thought that I didn't have a solution for this problem for a while, but then I realized that since I had been writing so many `Succ` classes, I might as well use this same method to label my product type arguments "a", "b", "c", and so on, and with this, I no longer had any excuses -- I must get to work on this.

## Converting Sum to a row type

First, let's start with our expected class, that we can determine a row type from a representation:

```hs
class SumToRow rep (row :: # Type)
  | rep -> row
```

From here, we can start matching on `Sum` to further get rows from the left and right sides, then union the row type results from the two to make the total row:

```hs
instance sumToRowSum ::
  ( SumToRow a r1
  , SumToRow b r2
  , Row.Union r1 r2 r
  ) => SumToRow (Sum a b) r
```

Looks normal so far. Then, we can unwrap the constructor to use the constructor name as our row labels, and keep the content of the constructor in the row:

```hs
instance sumToRowConstructor ::
  ( Row.Cons name a () r
  ) => SumToRow (Constructor name a) r
```

So since we are at a terminus for the sum type matching at the constructor, we can generically create our singleton row type by using `Row.Cons` with the field and an empty row type.

And that's it, I don't do anything more with the sum types here. If interested, one could further transform `NoArguments` to `Unit`, `Argument a` to `a`, and `Product a b` to `Tuple a b`, but I'm content to leave this as-is for now. We can define a convenience function to apply some proxies later:

```hs
sumToRow
  :: forall ty rep row
   . Generic ty rep
  => SumToRow rep row
  => Proxy ty
  -> RProxy row
sumToRow _ = RProxy
```

## AlphaSucc

Before we start with the product part, there's the problem with getting the labels I mentioned earlier. This ends up being a normal class where we have succeeding letters declared:

```hs
class AlphaSucc (curr :: Symbol) (next :: Symbol)
  | curr -> next

instance alphasuccab :: AlphaSucc "a" "b"
instance alphasuccbc :: AlphaSucc "b" "c"
instance alphasucccd :: AlphaSucc "c" "d"
-- ...
```

## Converting Product to a row type

When we work with product reps, we have the base constructor that we will unwrap. We'll also use this opportunity to feed in the initial character that we'll be using to label our row fields.

```hs
class ProductToRow rep (row :: # Type)
  | rep -> row

instance productToRowConstructor ::
  ( ProductToRowImpl rep "a" l row
  ) => ProductToRow (Constructor name rep) row
```

Then we can get to business, where we have one more step to perform in that we want to make sure that a given alphabet character only gets used once.

```hs
class ProductToRowImpl rep (label :: Symbol) (out :: Symbol) (row :: # Type)
  | rep label -> out row

instance productToRowImplProduct ::
  ( ProductToRowImpl a l l' r1
  , AlphaSucc l' l''
  , ProductToRowImpl b l'' out r2
  , Row.Union r1 r2 r
  ) => ProductToRowImpl (Product a b) l out r

instance productToRowImplArgument ::
  ( Row.Cons l a () r
  ) => ProductToRowImpl (Argument a) l l r
```

And this is about it, and we can define a convenience function:

```hs
productToRow
  :: forall ty rep row
   . Generic ty rep
  => ProductToRow rep row
  => Proxy ty
  -> RProxy row
productToRow _ = RProxy
```

## Usage

The actual application of these classes ends up being fairly simple:

```hs
data Fruit = Apple | Banana String | Kiwi Int
derive instance genericFruit :: Generic Fruit _

-- inferred type:
fruitRow :: RProxy
  ( "Apple" :: NoArguments
  , "Banana" :: Argument String
  , "Kiwi" :: Argument Int
  )
fruitRow = sumToRow (Proxy :: Proxy Fruit)

data Thing = Thing Int String Boolean
derive instance genericThing :: Generic Thing _

-- inferred type:
thingRow :: RProxy
  ( a :: Int
  , b :: String
  , c :: Boolean
  )
thingRow = productToRow (Proxy :: Proxy Thing)
```

## Accidentally interesting points

Right off the bat, we can see that working with the row type extracted from the sum type generic rep gives us a perfectly workable `Variant` that could also be expanded by being used in a `Row.Union` with another row type, given translation of the NoArg/Arg/Prod reps to something more directly usable such as Unit, singleton, Tuple.

In a similar way, converting the generic rep of product readily gives us the row type that we can apply to a record, though this is something much more commonly known. For any product types that don't need to implement a type class, it seems that we should prefer using a record instead.

## Conclusion

I hope this has shown how generics-rep information can be quite readily used for various purposes, even if not readily applicable to some problem in this particular form.

## Links

* Previous post about Generic Reps as types <https://qiita.com/kimagure/items/a5e340242f038b0dc748>
* This code <https://github.com/justinwoo/purescript-biang>

