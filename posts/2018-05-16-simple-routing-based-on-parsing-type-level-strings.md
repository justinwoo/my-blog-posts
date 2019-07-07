# Simple Routing based on parsing type-level strings

Last time, I talked about [Parsing type-level strings to extract types](https://qiita.com/kimagure/items/6729a5d55ab99bcee8ec) using some new features in PureScript 0.12 to be able to deconstruct `Symbol`s. This gives me back functions of `String -> Either _ { | params }` that I can use to parse URLs for parameters. But what if I wanted to do some routing?

This time, I'll write about a simple way to do routing by putting these routes into a row type, giving us back a `Variant` that we can match on.

## Row of `Type`s

While we would like to make rows directly of `# Symbol`, we should remember that `RowToList` and its kind `RowList` are `Type`-kinded, so we cannot use `RowToList` on a `# Symbol`. However, by using `SProxy`, we can carry around `Symbol`s to use in `# Type`. And just to make this easier on us, we can define a type alias to use:

```hs
type RouteURL = SProxy
```

## Top level

So at the top level, what we need is a `matchRoutes` function that will take in some kind of proxy (usually `RProxy`) of our `RouteURL (url :: Symbol)` fields to solve from these determined types the resulting `Variant`:

```hs
matchRoutes
  :: forall proxy routes routesL var
   . RL.RowToList routes routesL
  => RoutesLToVariant routesL var
  => proxy routes
  -> String
  -> Either NoMatch (Variant var)
matchRoutes _ = routesLToVariant (RLProxy :: RLProxy routesL)
```

where `NoMatch` is just a newtype of `String`, so parsing a given URL string with the function returned from providing the routes proxy will end up with either the error for no matching routes or a `Variant` of the route parameters. So as `RowToList` gives us our `routes` row type in iterable form, this is all we need to then solve for `var :: # Type`.

## `RoutesLToVariant`

Like usual, our type class will have a `RowList` parameter used to determine the `# Type` parameter:

```hs
class RoutesLToVariant
  (routesL :: RL.RowList)
  (var :: # Type)
  | routesL -> var
  where
    routesLToVariant
      :: RLProxy routesL
      -> String
      -> Either NoMatch (Variant var)
```

So that only `routesL` here is used to match instances, like we want.

First, let's look at the base instance when we've exhausted this list:

```hs
instance nilRoutesToVariant :: RoutesLToVariant RL.Nil ()
  where
    routesLToVariant _ s = Left (NoMatch s)
```

Here we have reached the `Nil`, so we can mark the row as being empty, as there are no more possible members to inject. Then we can just return our non-matched side with `NoMatch`.


For the `Cons` case, first let's look at the instance head:

```hs
instance consRoutesToVariant ::
  ( K.ParseURL url row
  , IsSymbol rName
  , RoutesLToVariant rTail var'
  , Row.Cons rName { | row } var' var
  , Row.Union var' var'' var
  ) => RoutesLToVariant
         (RL.Cons rName (SProxy url) rTail)
         var
```

By using the `ParseURL` class we defined last time, we can get out the row type of the parameter record from the URL, i.e. `ParseURL "/hello/{bill:String}" (bill :: String)`. From there, we can get the remaining variant row from the remaining `RowList`, and use `Row.Cons` to add our field of `rName` and `{ | row }` to the remaining row. The other constraints exist to aid some operations we need: we need to constrain `rName` with `IsSymbol` and declare that the remaining variant is some sub-type row for which an inferred complement will form the whole variant row.

And so, when we can define the instance method:

```hs
    routesLToVariant _ s =
      case K.parseURL (SProxy :: SProxy url) s of
        Right (r :: { | row }) ->
          Right $ Variant.inj (SProxy :: SProxy rName) r
        Left l ->
          Variant.expand <$> routesLToVariant (RLProxy :: RLProxy rTail) s
```

And the body here is the same as our usage last time, where if we have a match, we can return the variant member by injecting it with `rName`, and we can continue on with the remaining `RowList` to get back our sub-type variant, which we can map over and expand to be of the same type.

And that's it!

## Usage

First, let's set up a row of URLs we want to match on:

```hs
type RouteURLs =
  ( hello :: RouteURL "/hello/{name}"
  , age :: RouteURL "/age/{age:Int}"
  , answer :: RouteURL "/gimme/{item:String}/{count:Int}"
  )
```

When we then use this with `matchRoutes`, the inferred type will be concrete:

```hs
main = do
  let
    -- inferred type:
    -- (matchRoutes' :: String
    --     -> Either
    --          NoMatch
    --          (Variant
    --             ( hello :: { name :: String}
    --             , age :: { age :: Int }
    --             , answer :: { item :: String , count :: Int }
    --             )
    --         )
    -- ) =
    matchRoutes' =
      matchRoutes (RProxy :: RProxy RouteURLs)
```

And from there, it's just a matter of applying a bunch of inputs and matching against them:

```hs
    testRoutes =
      [ "/hello/Bill"
      , "/age/12"
      , "/gimme/Apple/24"
      , "/no/match"
      ]

    results = matchRoutes' <$> testRoutes
    handleResult = case _ of
      Left (NoMatch l) ->
        log $ "no match for: " <> show l
      Right r ->
        Variant.match
          { hello: \x -> log $ "hello your name is " <> x.name
          , age: \x -> log $ "hello you are " <> show x.age
          , answer: \x -> log $ "you want " <> show x.count <> " of " <> x.item
          }
          r

  traverse_ handleResult results

  -- result:
  -- hello your name is Bill
  -- hello you are 12
  -- you want 24 of Apple
  -- no match for: "/no/match"
```

And as expected, our results came back where the three valid routes were matched with the correct types and handled by the `Variant.match`, and our invalid case came out on the left.

## Conclusion

Hopefully this has shown you that even with the new features in 0.12, we can reapply the same techniques from 0.11.x to seamlessly weave together different features to solve our problems.

## Links

* this code, as a library: <https://github.com/justinwoo/purescript-hibachi>
* my previous post, "Parsing type-level strings to extract types": <https://qiita.com/kimagure/items/6729a5d55ab99bcee8ec>