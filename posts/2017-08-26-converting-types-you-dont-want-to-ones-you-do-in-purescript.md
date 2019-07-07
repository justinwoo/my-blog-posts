# Converting types you don't want to ones you do in Purescript

## Update

As of PureScript 0.12, we no longer need any of these tricks as Generics-Rep will not derive or use `Record` and `Field`, which is good since we can do everything with records directly using `RowToList`!

-----

Recently, I've been writing a lot of code that uses information from the type level to do lots of generic operations. It's been a lot of fun, but I've found that there's not really enough out there to explain how people might work with types at the type level.

## Example problem

While working on my [purescript-bundaegi](https://github.com/justinwoo/purescript-bundaegi) demo, I ran into the problem that even though I already wrote instances to work with `Record row`, I also needed to write some functions that worked on Generic Reps. Generic Reps produce a record structure represented with a `Rec` constructor with fields that `Product`s of `Field`. For example,

```hs
data Person = Person 
  { first   :: String
  , last    :: String
  , address :: Address
  }
derive instance genericPerson :: Generic Person _
```

Has the type-level Generic Rep of

```hs
Constructor
  "Person"
  (Rec
    (Product
      (Field "address" Address)
      (Product
        (Field "first" String)
        (Field "last" String))))
```

But I need to convert `Rec fields` into `Record row` somehow if I want to reuse my record instances. How?

## Define a typeclass!

We can define a typeclass for exactly this like so:

```hs
class FieldsToRow fields (row :: # Type)
```

Such that for any given type, we can get the corresponding row type. "Wait, this can't work", you might say, but I have code that does exactly this:

```hs
-- type class for a generic rep having a Typescript representation
instance recHasTSRep ::
  ( FieldsToRow fields row
  , HasTSRep (Record row)
  ) => HasTSRep (Rec fields) where
  -- ...
```

## Writing our instances

Since we know that the only types we want to handle are `Field` and `Product`, this gets a lot easier than one might initially think. For the `Product` case, the instance can be defined like so:

```hs
instance productFieldsToRow ::
  ( FieldsToRow a l
  , FieldsToRow b r
  , Union l r row
  ) => FieldsToRow (Product a b) row
```

From the left side, we're able to get the row type, and the same for the right. And then we use the [`Union`](https://pursuit.purescript.org/builtins/docs/Prim#t:Union) class to union the rows together.

Then we have our `Field` instance:

```hs
instance fieldFieldsToRow ::
  ( RowCons name ty () row
  ) => FieldsToRow (Field name ty) row
```

...which uses the [`RowCons`](https://pursuit.purescript.org/builtins/docs/Prim#t:RowCons) class to create singleton rows by adding our field `name :: ty` to an empty row.

That's it!

## Another example

I also had a case where I needed to convert a generics-rep `Sum` and its `Constructor`s to a `RowList`. This also ends up being a matter of defining a class and some instances:

```hs
class GenericSumToRowList a (rl :: RowList)
```

```hs
instance sumGenericSumToRowList ::
  ( GenericSumToRowList a l
  , GenericSumToRowList b r
  , RowListAppend l r rl
  ) => GenericSumToRowList (Sum a b) rl
```

...where [`RowListAppend`](https://github.com/LiamGoodacre/purescript-typelevel-prelude/blob/7fba5aab064f4f3b7fb05a183404abcca4d7d84d/src/Type/Row.purs#L131) is a class Liam Goodacre wrote for appending two `RowList`s.

As for the `Constructor` case:

```hs
instance constructorGenericSumToRowList ::
  ( TypeEquals (RLProxy (Cons name ty Nil)) (RLProxy rl)
  ) => GenericSumToRowList (Constructor name ty) rl
```

In our instance here, I created a `RowList` with a single element by using the `Cons` data type similar to the `FieldsToRow` base case. To then equate this to `rl`, I used `RLProxy` to make `Type`s out of the row lists so I could use `TypeEquals` for equality. Typeclasses all the way down!

## Conclusion

Hopefully, this post has shown you that type classes can convert types you don't want to use to ones that you do want to use, and that you can define your own typeclasses to do conversions as you need.

## Links

* My Typescript types + io-ts schema generation demo https://github.com/justinwoo/purescript-bundaegi