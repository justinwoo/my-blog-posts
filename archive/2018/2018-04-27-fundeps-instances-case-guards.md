# Type classes and instances are pattern matching for types

Recently, I had the opportunity to speak at [NY PureScript](https://www.meetup.com/ny-purescript/events/249706177/) at the incredible [10 Hudson Yards](https://en.wikipedia.org/wiki/10_Hudson_Yards) building. At the meetup, there were some questions about how function dependencies and instance heads work, with some small chat about how there's a parallel between instance heads and case expressions, with fundeps determining which terms are literally matched for and which are not. While it's not quite 1:1, I thought there might be worth writing about, as "type level programming" doesn't need to be "magic" or anything -- it's a lot of the same things being applied just one level up.

## Example Problem

Let's define a class that will take a `RowList` to iterate on, a `# Type` of a record input, and a `Type` that we'll use to guarantee that the record is homogeneous:

```hs
class Values (xs :: RowList) (row :: # Type) a
  | xs -> row a
  where
    values :: RLProxy xs -> { | row } -> List a
```

The fundep `xs -> row a` is then used for matching instances, where only the `xs :: RowList` will be used to match instances.

### Nil

So how do I write a `Nil` instance of this class usually? I usually put the row parameter directly in the instance head:

```hs
instance nilValues ::
  Values Row.Nil () a where
  values _ _ = List.Nil
```

But since the `()` here from the row parameter isn't used for matching the instance, it's the same as if we instead put it in a constraint:

```hs
instance nilValues ::
  ( TypeEquals { | row } {}
  ) => Values Row.Nil row a where
  values _ _ = List.Nil
```

Or if we prefer this in a prettified version using the same record equality,

```hs
class TypeEquals {|r1} {|r2}
   <= RowEquals (r1 :: # Type) (r2 :: # Type)
```

Which is similar to if you had a similar term level definition for a partial function:

```hs
matchEmpty :: Partial => List Int -> StrMap Int -> Boolean
matchEmpty xs r = case xs, r of
  List.Nil, empty' ->
    false
  where
    empty' = StrMap.empty
```

Which could be written by using a guard instead:

```hs
matchEmpty :: Partial => List Int -> StrMap Int -> Boolean
matchEmpty xs r = case xs, r of
  List.Nil, r' | r' == empty' ->
    false
  where
    empty' = StrMap.empty
```

So hopefully this makes sense, that constraints on instances are like guards in cases in many ways. The difference being that while guards can use the extra predicates and continue down the cases, a failed constraint will lead to a compile-time error. *(There's also some parallels to instance chains but that's not important to discuss here)*

In addition, type classes in this analogy end up being partial functions for types, where if you have correctly matching instances, then the evaluated function in compile time terminates and you have a successful build. Case expressions, on the other hand, can be made `Partial`, but your programs crashing in runtime from partiality is some of the worst things you can run into. As PureScript users, we're totally fine and really prefer that we have these partial functions in compile time, and there's even `Error` constraints you can use to display errors for specific cases, unsafely crashing the compilation.

### Cons

So continuing on, we can write our Cons instance that will take a value of the record.

```hs
instance consValues ::
  ( IsSymbol name
  , Values tail row a
  , RowCons name a row' row
  , RowLacks name row'
  ) => Values (Row.Cons name a tail) row a where
  values _ r = List.Cons first rest
    where
      nameP = SProxy :: SProxy name
      first = Record.get nameP r
      r' = Record.delete nameP r :: { | row' }
      rest = values (RLProxy :: RLProxy tail) r'
```

Like before, we can choose to use constraints for the type in the Cons cell:

```hs
instance consValues ::
  ( IsSymbol name
  , Values tail row a
  , TypeEquals a ty
  , RowCons name ty row' row
  , RowLacks name row'
  ) => Values (Row.Cons name ty tail) row a where
  values _ r = List.Cons first rest
    where
      nameP = SProxy :: SProxy name
      -- needed if we don't match the type in the instance head
      -- because then we need to apply the type equality
      first = Equality.from $ Record.get nameP r
      r' = Record.delete nameP r :: { | row' }
      rest = values (RLProxy :: RLProxy tail) r'
```

But you can see that now because we are using `TypeEquals a ty`, we need to use `from` to apply the casting of the `ty`-typed value we get from our record to `a` for the retruned `List a` of our type class.

And the equivalent in the term-level would be that we apply extra guards to the values we work with, so a function that works with a `List Int` of only `1` values being allowed would look like this:

```hs
addOnesPartial :: Partial => Int -> List Int -> Int
addOnesPartial acc xs = case xs of
  List.Cons x tail
    | x == 1
    -> addOnes (x + acc) tail

  -- or match with the literal value like in instance heads
  -- List.Cons 1 tail
  --   -> addOnes (1 + acc) tail

  List.Nil -> acc
```

But like before, this is pretty terrible to deal with in runtime, so we're much better off either validating the list beforehand or filtering the items as we go:

```hs
addOnes :: Int -> List Int -> Int
addOnes acc xs = case xs of
  List.Cons x tail
    | x == 1
    -> addOnes (x + acc) tail
  List.Cons x tail
    -> addOnes acc tail
  List.Nil -> acc
```

### Running these examples

Let's run some examples of addOnesPartial:

```hs
  -- 1
  log $ show $ addOnesPartial 0 (List.Cons 1 List.Nil)

  -- crashes with partiality
  -- log $ show $ addOnesPartial 0 (List.Cons 2 List.Nil)
```

So as expected, since we defined our partial function to only work with values of `1`, feeding in `2` gives us a crash. Hardly surprising when you have a `Partial` constraint on the function.

Then let's see some uses of values, where we first need to define a helper:

```hs
values'
  :: forall xs row a
   . RowToList row xs
  => Values xs row a
  => { | row }
  -> List a
values' = values (RLProxy :: RLProxy xs)
```

So this will work with any concretely typed record being passed in where the produced `RowList` works with `Values`. Let's see an example:

```hs
main = do
  -- (1 : 2 : Nil)
  log $ show $ values' {x: 1, y: 2}
```

So this works as expected as our record is homogeneous. But what if we feed in an invalid one?

```hs
  -- compile error: can't match Int with String when solving
  log $ show $ values' {x: "A", y: 2}
```

In this case, we'll get a compile-time errror about this:


```
  Could not match type

    Int -- ran into an Int when

  with type

    String -- String was expected


while solving type class constraint

  Main.Values (Cons "y" Int Nil) -- the field from the RowList
              t3 -- for some inferred type t3
              String -- String was the type of a in our constraint

while applying a function values' -- our function and its inferred types:
  of type RowToList t0 t1 => Values t1 t0 t2 => { | t0 } -> List t2
  to argument { x: "A" -- the argument we passed in
              , y: 2
              }
while inferring the type of values' { x: "A"
                                    , y: 2
                                    }
in value declaration main
```

So if we sit down and read the error message, it actually does tell us quite a bit of what we needed to know. Of course, when we run into this error, it's a bit confusing if we don't then go to the instance head and look at the constraints there, but that could be solved with either more documentation or some upcoming instance chains features to chain the instance to an instance that can give us clearer errors.


## Conclusion

Hopefully this has shown you that there are quite a lot of parallels between instance matching and case expressions. After reading this post, I hope the similarity of using a type class of kind `RowList` where the data types associated are `Nil :: RowList` and `Cons :: Symbol -> Type -> RowList -> RowList` and a case expression of `List a` where the constructors associated are `Nil :: List a` and `Cons :: a -> List a -> List a` become more clear.

## Links

* This repo: <https://github.com/justinwoo/fundeps-instances-sum-case-guards-demo>
* <https://leanpub.com/purescript/read#leanpub-auto-type-classes>
* <https://leanpub.com/purescript/read#leanpub-auto-functional-dependencies>

