---
title: Short:  Composed Modified JSON parsing for free with Simple-JSON
tags: purescript
author: kimagure
slide: false
---
Previously, I wrote about how you can get modified JSON parsing for free with Simple-JSON [here](https://qiita.com/kimagure/items/801e1c55d4f8f218f11e). This post took advantage of the first-class nature of record types and the associated row types, using Record operations to insert, rename, and modify fields as needed, with the constraints defining what records these operations should work on, defining the record type that should be used to parse from by context.

While I talked about using Builder shortly, I didn't have any examples there yet. So this time I'll go into an example of how the Builder functions work.

## What is Builder?

In PureScript-Record there is a newtype [Builder](https://pursuit.purescript.org/packages/purescript-record/0.2.5/docs/Data.Record.Builder#t:Builder) which allows for composing together multiple operations to be performed on a record. The reason you'd use this is not only to be able to compose operations through various ways like with type class instances (e.g. [my type-level URL params demo](https://qiita.com/kimagure/items/4f5c6054870f631ff768#parsing-the-url-using-our-tuple)), but to overall avoid the cost of intermediate representations of records and cumbersome to deal with when bound to temporary variables.

## Usage

Like my previous examples, I set up a target record type that I wanted to parse to and some test JSON:

```hs
type MyRecord =
  { apple :: String
  , banana :: Array Int
  , cherry :: String
  }

testJSON :: String
testJSON = """
{
  "banana": null,
  "grape": "originally a grape"
}
"""
```

We can see the test JSON is "wrong" in multiple ways:

* it doesn't have an "apple" field defined
* "banana" is null when it should be an array
* it doesn't have a "cherry" field defined
* it has a "grape" field that will be thrown away

In this case, we could fix this by applying the following three fixes:

* always modify "banana" to take the nullable and either get a value or default it to an empty array
* rename "grape" to "cherry"
* insert an "apple" field with some string value

And so we can define builders for these operations and compose them, and run use `build` to apply the composed builder to the record type that we parse from:

```hs
parseMyRecord :: String -> Either (NonEmptyList ForeignError) MyRecord
parseMyRecord s = do
  let
    builder
        = Builder.modify (SProxy :: SProxy "banana") (fromMaybe [] <<< toMaybe)
      <<< Builder.rename (SProxy :: SProxy "grape") (SProxy :: SProxy "cherry")
      <<< Builder.insert (SProxy :: SProxy "apple") "i wasn't invited"
  obj <- readJSON s
  pure $ Builder.build builder obj
```

And so the final builder we get takes a record `{banana :: Nullable (Array Int), grape :: String}` and returns `MyRecord`. And so in action:

```hs
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  case parseMyRecord testJSON of
    Right {apple, banana, cherry} ->
      log $ "apple: " <> apple <> ", banana: " <> show banana <> ", cherry: " <> cherry
    Left e ->
      log $ "failed to parse: " <> show e

  -- result
  -- apple: i wasn't invited, banana: [], cherry: originally a grape
```

## Conclusion

Hopefully this has shown that Record.Builder is quite useful and easy to use for composing a bunch of operations that need to be performed on a record, and by using Builder in conjunction with Simple-JSON, you can get concretely typed parsing functions that perform the error-correction and additional validation you want without needing to do much extra work.

## P.S.

Builder is how Simple-JSON's handling of records and their fields is implemented too: https://github.com/justinwoo/purescript-simple-json/blob/de906d725b83cdf9bc5166c1747206e816a94263/src/Simple/JSON.purs#L134

## Links

* Repo: https://github.com/justinwoo/multiple-modifications-with-builder-simple-json-example
* PureScript.Record.Builder: https://pursuit.purescript.org/packages/purescript-record/docs/Data.Record.Builder

