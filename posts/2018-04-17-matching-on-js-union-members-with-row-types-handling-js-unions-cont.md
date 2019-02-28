The other day, I wrote about handling JS Unions with Row Types, by using a constructorless data type with a row parameter to note what labels I want to associate members with, with guard functions for how you would unsafely guard on these types. The `unsafeGuardMember` function described in that post gave us a way to eliminate members of the union until we are ultimately left with no more tracked union members (which very well happens in many uses of JS Unions).

This time, I'll continue with just a small extension to implement a `match` function.

## Short review

Before we start, let's review the definitions of `JSUnion` and `UnsafeGuardFor`.

```hs
data JSUnion (members :: # Type)
```

Purposefully, the JSUnion cannot be constructed directly, but the `members` parameter being of kind `# Type` gives us a way to work with the possible values inside the union.

```hs
newtype UnsafeGuardFor (name :: Symbol) ty =
  UnsafeGuardFor (Foreign -> Boolean)
```

For a given field in our JSUnion, it should be feasible to implement a guard function that explicitly declares what type is to be extracted, using a test of `Foreign -> Boolean`.

## class MatchMembers

With this information, we can define a class where we can iterate the `members` row type using `RowToList`. We can define this in terms of the `RowList` being iterated, `members` of the JS Union, a record of `pairs` that will contain the guard and an associated function to be applied, and a `result` type for the result of the functions being applied in the matching. And since we know that we may not have enough information to exhaustively check for the JS Union members (as is the case with any dynamically typed "outside world" value), the class method should return a `Maybe result`, where the Nothing will be returned if nothing has been found.

Put into a class definition, we end up with this:

```hs
class MatchMembers (xs :: RowList) (members :: # Type) (pairs :: # Type) result
  | xs result -> members pairs
  where
    matchMembers :: RLProxy xs -> { | pairs } -> JSUnion members -> Maybe result
```

Where the fundep `xs result -> members pairs` exists primarily because row types can't be used in instance heads for matching. Otherwise, everything else is in order here.

*I should probably mention here that maybe you actually are sure that you have exhaustively listed the possible members of your JS Union. In that case, you may want to use a coerced `fromJust` function, but it's your fault if it breaks.*

Then our instances put concretely what we have described above. Starting with `Nil`:

```hs
instance matchMembersNil :: MatchMembers Nil members pairs result where
  matchMembers _ _ _ = Nothing
```

## matchMembersCons

As with most `RowList` classes, our `Cons` instance contains the meat of what we want to accomplish. The one decision that needs to be made here is what kind of concrete type we want the `pairs` to have. I chose to go with the easy choice of using the `Tuple` type. From there, the rest of the instance constraints fall in line, where from `Cons name ty tail`, `name` is declared as a known `Symbol`, `members` is declared to have `name ty` as a field, and `pairs` is declared to have a tuple of the guard and the result function:

```hs
instance matchMembersCons ::
  ( IsSymbol name
  , RowCons name ty members' members
  , RowCons name (Tuple (UnsafeGuardFor name ty) (ty -> result)) pairs' pairs
  , MatchMembers tail members pairs result
  ) => MatchMembers (Cons name ty tail) members pairs result where
```

And as usual, the `tail` is then used to continue the instances through the list. The method body then has nothing more than application of the `unsafeGuardMember` function defined previously:

```hs
  matchMembers _ pairs union =
    case unsafeGuardMember unsafeGuard union of
      Right x -> Just $ fn x
      Left _ -> matchMembers (RLProxy :: RLProxy tail) pairs union
    where
      nameP = SProxy :: SProxy name
      Tuple unsafeGuard fn = Record.get nameP pairs
```

And so using the `RowCons` constraint for `pairs` from above, we grab the `Tuple unsafeGuard fn` from `{ | pairs }`.

## matchJSUnion

Then all this needs is a nice top-level function to be called with, so we define it almost verbatim minus the `RLProxy` argument that we can infer the type for:

```hs
matchJSUnion
  :: forall members xs pairs result
   . RowToList members xs
  => MatchMembers xs members pairs result
  => { | pairs }
  -> JSUnion members
  -> Maybe result
matchJSUnion =
  matchMembers (RLProxy :: RLProxy xs)
```

And that's it!

## Usage

First, let's see a usage that matches on a valid specified JS Union member.

```hs
    T.test "matchMembers 1" do
      let
        (union :: TestUnion) = H.fromMember countP 1
        match = H.matchJSUnion
          { count: Tuple countGuard show
          , name: Tuple nameGuard id
          }
          union
      case match of
        Just value -> do
          Assert.equal value "1"
        Nothing ->
          T.failure "incorrect result from matchJSUnion"
```

Cool, so this works as expected, where the `count` guard correctly matches and we get `Just "1""` as a result of `show` on the integer `1`.

Next, let's see a typical case where some value doesn't match any of the cases we're handling.

```hs
    T.test "matchMembers 2" do
      let
        (union :: TestUnion) = unsafeCoerce { crap: "some bullshit from JS" }
        match = H.matchJSUnion
          { count: Tuple countGuard show
          , name: Tuple nameGuard id
          }
          union
      case match of
        Just value -> do
          T.failure "incorrect result from matchJSUnion"
        Nothing ->
          T.success
```

And so, we correctly get `Nothing` as a result of not matching on anything here. And yes, while this example seems readily obvious, people forget about cases when "exhaustive" checks are not actually exhaustive, so this actually is quite important to have.

Otherwise, if you are truly working with a closed set of members, then you could use `fromJust` here.

## Conclusion

Hopefully this has shown that with a little bit of normal `RowToList` usage, you can provide yourself more convenient and correct ways of modeling problems and working with them.

If you're at all interested in using this library also, ping me sometime and I'll be more active about publishing and maintaining it.

## Links

* PureScript-Hotteok: <https://github.com/justinwoo/purescript-hotteok>
* Previous blog post, "Handling JS Unions with Row Types": <https://qiita.com/kimagure/items/141423771ad1f5a84425>