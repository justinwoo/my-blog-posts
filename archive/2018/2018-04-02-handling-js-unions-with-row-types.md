# Handling JS Unions with Row Types

Normally, when we deal with values that come from the JS world (via some JS library or something), we use the Foreign type and read the value using various well-typed functions with concrete failures, usually in some form of `Foreign -> Either Error WhatIWant`.

And while this works for various things, we also sometimes need to deal with various kinds of values that might be one or many types. While we can try to wrap these in various redundant FFI functions (and we probably should), we can also come up with solutions for working with these directly.

## Unions in general

*(No, not sum types/tagged unions of constructors, as confusingly named in some communities)*

The general approach for working with unions in "gradually typed" environments seems to follow along these steps:

1. Identify what a given value's type may be, as a union of members
2. For a given member, allow users to either use a predefined unsafe type guarding function or define their own to discriminate members of the union, passing along a subset of the total union.
3. Expose various ways to work with these type guards.

Some approaches also seem to allow for named members, where the labels are only used in compilation, while many are based on simple nested unions of the types themselves. Well, to make things easier for usage and to leverage existing type classes, row types make a better fit here.

## Defining our types

So to represent this union, I define a data type with no constructor (so that I don't incorrectly try to construct it) with a row type parameter for what the members of the union are:

```hs
data JSUnion (members :: # Type)
```

And just as a refresher, `# Type` stands for an unordered set of `Symbol` - `Type` pairs, e.g. `(apple :: String, banana :: Int)` is equivalent to `(banana :: Int, apple :: String)`.

And for the guard function, I provide the `Symbol` and `Type` as parameters, where the function used should be able to tell from a `Foreign` value if the value is of the correct type or not:

```hs
newtype UnsafeGuardFor (name :: Symbol) ty =
  UnsafeGuardFor (Foreign -> Boolean)
```

And that's it for the types we need to define.

## Creating a JSUnion from a member

We can first define a function for how to create the JSUnion from a member value. We know that we'll need the `Symbol` name and `Type` type of the member, but we also need to add the constraint that this member is actually a member of our JSUnion, which is accomplished by using `RowCons`.

```hs
fromMember
  :: forall name ty members' members
   . RowCons name ty members' members
  => SProxy name
  -> ty
  -> JSUnion members
fromMember _ x = unsafeCoerce x
```

The constraint applied here states that there is some sub-row of `members` that we have assigned to `members'`, where adding a field of `name` and `ty` would form the `members` row. Illustrated:

![](https://i.imgur.com/4t8kgKR.png)

And as we have applied a constraint that checks our types for the properties we want, we can then coerce our value into the union as desired. 

## Narrowing down members of JSUnion

The most important feature is then to be able to discriminate members of our JSUnion, and get back an Either of the JSUnion with the current field removed and the value extracted for a match.

```hs
unsafeGuardMember
  :: forall name ty members' members
   . RowCons name ty members' members
  => UnsafeGuardFor name ty
  -> JSUnion members
  -> Either (JSUnion members') ty
unsafeGuardMember (UnsafeGuardFor check) jsUnion =
  if check (toForeign jsUnion)
     then Right (unsafeCoerce jsUnion)
     else Left (unsafeCoerce jsUnion)
```

Very much applying the same as before. We could add better types here for my coercions, since we know that in the Right branch we want `ty` and the Left `JSUnion members'`, but that can be looked at later in any case.

Then for convenience, we can also provide an extra unsafe coercion helper:

```hs
unsafeCoerceMember
  :: forall name ty members' members
   . RowCons name ty members' members
  => SProxy name
  -> JSUnion members
  -> ty
unsafeCoerceMember _ x =
  unsafeCoerce x
```

But at least there's a check that the coercion is done using a name and type pair that exists in the JSUnion.

Finally, for the case that we have exhaustively whittled down all members save for one and we know for certain that there are no unguarded/unknown members, we can then extract the remaining singleton. And for this, we can't just apply `RowCons` if we want to be able to extract it without providing the name, as the functional dependency sets require the label to be determined:

```hs
class RowCons (l :: Symbol) (a :: Type)
              (i :: # Type) (o :: # Type)
  | l a i -> o
  , l o -> a i
```

Thankfully, since we know what the row type parameter is in these contexts, we can just put our old friend `RowToList` to work and match against a 1-length `RowList`:

```hs
unsafeExtractSingleton
  :: forall members name ty
   . RowToList members (Cons name ty Nil)
  => JSUnion members
  -> ty
unsafeExtractSingleton =
  unsafeCoerce
```

And these are actually all the methods we need.

## Putting JSUnions to work

For our tests, we'll define a simple JSUnion of two members, proxies for the labels, and a simple example guard:

```hs
import Hotteok as H

type TestUnion = H.JSUnion
  ( name :: String
  , count :: Int
  )

nameP = SProxy :: SProxy "name"
countP = SProxy :: SProxy "count"

countGuard :: H.UnsafeGuardFor "count" Int
countGuard =
  H.UnsafeGuardFor
      $ isRight
    <<< runExcept
    <<< readInt
```

Then some test cases that follow what we expect. First, with `fromMember` type checking as expected:

```hs
    T.test "fromMember" do
      let (union :: TestUnion) = H.fromMember nameP "banana"
      T.success
```

Then, a test to see if unsafeCoerceMember works as expected:

```hs
    T.test "unsafeCoerceMember" do
      let
        (union :: TestUnion) = H.fromMember nameP "banana"
        value = H.unsafeCoerceMember nameP union
      Assert.equal value "banana"
```

And one for unsafeExtractSingleton, where we type this to specifically only `(name :: String)`:

```hs
    T.test "unsafeExtractSingleton" do
      let
        (union :: H.JSUnion (name :: String)) = H.fromMember nameP "banana"
        value = H.unsafeExtractSingleton union
      Assert.equal value "banana"
```

Then the first test for unsafeGuardMember, to see if a failed guard then can be singleton-extracted correctly:

```hs
    T.test "unsafeGuardMember 1" do
      let
        (union :: TestUnion) = H.fromMember nameP "banana"
        guarded = H.unsafeGuardMember countGuard union
      case guarded of
        Right e ->
          T.failure "incorrect branch from JSUnion"
        Left singleton -> do
          let value = H.unsafeExtractSingleton singleton
          Assert.equal value "banana"
```

And then one to see the case when the guard gives us back the value:

```hs
    T.test "unsafeGuardMember 2" do
      let
        (union :: TestUnion) = H.fromMember countP 1
        guarded = H.unsafeGuardMember countGuard union
      case guarded of
        Right value -> do
          Assert.equal value 1
        Left e ->
          T.failure "incorrect branch from JSUnion"
```

Overall quite normal, but still worth the sanity check.

## Using Chalk

So as an example, we can try calling chalk. In FFI, we can just import in the module:

```js
exports.chalk = require("chalk");
```

Then we can roughly type the bits we care about in the foreign import:

```hs
foreign import chalk
  :: { blue
       :: H.JSUnion
            ( fn :: String -> String
            , obj :: { bgYellow :: String -> String }
            )
     } 
```

Such that the `blue` property of chalk can be called as a function for applying blue text coloring, and it can also be used as a record to get the `bgYellow` function to call to apply blue text and yellow background coloring. Then the usage is quite straightforward, where we can coerce this to what we want:

```hs
  let
    fnP = SProxy :: SProxy "fn"
    objP = SProxy :: SProxy "obj"
    blueFn = H.unsafeCoerceMember fnP chalk.blue
    blueObj = H.unsafeCoerceMember objP chalk.blue
  log $ blueFn "blue text"
  log $ blueObj.bgYellow "yellow background blue text"
```

And the result:

![](https://i.imgur.com/EROXLV5.png)

## Conclusion

Hopefully this has shown you one way you might work with JS unions. While you would probably much rather prefer to use something like Simple-JSON or Foreign directly and be able to get real validation of things that you're working with, for things like Chalk, you'll have to have some way of dealing with the functions that you need to call.

If anything, hopefully this shows you some of the ways you can use row types and constraints on them to create well-typed interfaces for undesirable ones.

*And yes, this is basically the most interesting part of TypeScript implemented as a plain PureScript library.*

## Links

* Repo: https://github.com/justinwoo/purescript-hotteok

