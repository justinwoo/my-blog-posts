# Automatically de/encoding JSON in Purescript using Generics-Rep

I only recently realized that I still did not write a blog post about this, despite having an Egghead video and having given a small talk about it. With the release of the new version of Foreign-Generics, I figured I might as well write about the normal usage and about the new Enum-style Sum type Generic functions.

## What

I think most of us work often with JSON in some capacity, using tools available in C#/Java, some more obscure tools like [io-ts](https://github.com/gcanti/io-ts) in Typescript, unchecked JSON.parse into Object/Hashmap soup, manual decoding parsers, code generators for parsers, or others -- with the lucky ones of you already using some approach through libraries like Aeson in Haskell, Argonaut-Shapeless in Scala, or something else.

Here are just some of the reasons you might use an automatic approach:

1. For correctness -- to avoid switching order of constructor arguments or using the wrong property string key for parsing.
2. For saving time -- why write something manually when it can be done automatically? Not to mention the cost of every data model change...
3. For fun!

*In the exceptional cases when a generated instance wouldn't work, every automatic approach gives you tools to provide an implementation manually, so there's no "lock-in".*

Let's see how this works in Purescript:

## How

In Purescript, [Foreign-Generic](https://github.com/paf31/purescript-foreign-generic/) lets us take any data type with a `Generic`s-Rep instance and instance `Decode` and `Encode` instances using the `genericDecode` and `genericEncode` methods. Let's go through this in parts:

### Deriving Generics-Rep

First, we'll define our type and derive `Generic`.

```hs
newtype SimpleRecord = SimpleRecord
  { a :: Int
  , b :: String
  , c :: Boolean
  }
derive instance repGenericSimpleRecord :: Generic SimpleRecord _
```

Here, we've defined a newtype data type of a couple fields, and derived `Generic` using the compiler's automatic Generics-Rep derivation. Note the underscore at the end, which is part of the Generic type class signature for the actual representation being used.

### Defining the Decode instance

The `Decode` type class is used to define how foreign JS objects should be decoded to our type with the `decode` method. We can use the `genericDecode` to define it:

```hs
instance decodeSimpleRecord :: Decode SimpleRecord where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
```

As `genericDecode` takes an `Option`, we provide the `defaultOptions` provided, but with `unwrapSingleConstructors` set to true, which will skip matching the constructor tag. We'll see why this option is useful in decoding sum types later, but most of our instances will use this for decoding/encoding to "normal" JS objects.

### Defining the Encode instance

Similar to the above, the `Encode` type class lets us define how to convert our type into a JS object using the `encode` method. We use the `genericEncode` method in the same way as above.

```hs
instance encodeSimpleRecord :: Encode SimpleRecord where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
```

Not much new here!

### Putting this to work

Encoding is a failure-free operation, so encoding JSON has the signature `encodeJSON :: forall a. Encode a => a -> String`. However, decoding is an operation that can fail with a number of different attempts to parse, and so decoding JSON has the signature `decodeJSON :: forall a. Decode a => String -> F a`, where `type F = Except MultipleErrors`. We can readily convert this into an `Either` to show it by defining `decodeJSON' = runExcept <<< decodeJSON`.

Let's use this function `testJSON` that will take our data to test a round trip en/decode and test decoding a JSON string:

```hs
testJSON original input expected = do
  log' "can be converted to JSON"
    (show original) json
  it "can be converted back" $
    decodeJSON' json `shouldEqual` Right original
  it' "can be converted from JSON" input expected $
    decodeJSON' input `shouldEqual` expected
  where
    decodeJSON' = runExcept <<< decodeJSON
    json = encodeJSON $ original
    format a b c = a <> "\n    " <> b <> "\n    -> " <> c
    log' t a b = it (format t a b) $ pure unit
    it' a b c t = it (format a b $ show c) t
```

Then for the given usage for our simple record:

```hs
describe "SimpleRecord" do
  testJSON
    (SimpleRecord { a: 1, b: "b", c: true })
    """{ "a": 123, "b": "abc", "c": false }"""
    (Right (SimpleRecord { a: 123, b: "abc", c: false }))
```

We will get back results that look like this:

```hs
SimpleRecord
  ✓︎ can be converted to JSON
    (SimpleRecord { a: 1, b: "b", c: true })
    -> {"c":true,"b":"b","a":1}
  ✓︎ can be converted back
  ✓︎ can be converted from JSON
    { "a": 123, "b": "abc", "c": false }
    -> (Right (SimpleRecord { a: 123, b: "abc", c: false }))
```

And so we know it works! Let's explore some other forms too.

## Enum-style Sum types

We often work with JSON that has string literals encoded in a single field. We've added some methods for sum types where the constructors have no arguments, so that you can work with these easily:

```hs
data Fruit
  = Apple
  | Banana
  | Watermelon
derive instance repGenericFruit :: Generic Fruit _
-- since Fruit is an enum-style sum type (i.e. it's a sum type
-- of constructors with no arguments, we can use genericDecodeEnum 
-- and genericEncodeEnum!)

instance decodeFruit :: Decode Fruit where
  decode = genericDecodeEnum { constructorTagTransform: id }
-- we could provide toUpper or something here if needed

instance encodeFruit :: Encode Fruit where
  encode = genericEncodeEnum { constructorTagTransform: id }
```

And so:

```hs
describe "Fruit - Enum style ADT" do
  testJSON
    (Apple)
    "\"Watermelon\""
    (Right Watermelon)
```

```hs
Fruit - Enum style ADT
  ✓︎ can be converted to JSON
    Apple
    -> "Apple"
  ✓︎ can be converted back
  ✓︎ can be converted from JSON
    "Watermelon"
    -> (Right Watermelon)
```

## Sum types containing Constructors with Arguments

We also often work with sum types that have been encoded in JSON by using a tag field to note which constructor/tag has been encoded. These work out of the box with the default options:

```hs
data ADTWithArgs
  = Increment
  | Add Int
  | Set { count :: Int }
  | Reset
instance decodeADTWithArgs :: Decode ADTWithArgs where
  decode = genericDecode defaultOptions
instance encodeADTWithArgs :: Encode ADTWithArgs where
  encode = genericEncode defaultOptions
```

```hs
describe "ADTWithArgs" do
  testJSON
    (Set { count: 5 })
    """{ "tag": "Add", "contents": 123 }"""
    (Right (Add 123))
```

```hs
ADTWithArgs
  ✓︎ can be converted to JSON
    (Set { count: 5 })
    -> {"contents":{"count":5},"tag":"Set"}
  ✓︎ can be converted back
  ✓︎ can be converted from JSON
    { "tag": "Add", "contents": 123 }
    -> (Right (Add 123))
```

It works!

## Null/Undefined fields

We end up with JSON where fields may be optional, and these are encoded often by writing `null` or by not writing the field at all (undefined). We provide `NullOrUndefined`, which is a newtype for Maybe that explicitly defines this behavior:

```hs
newtype RecordWithArrayAndNullOrUndefined = RecordWithArrayAndNullOrUndefined
  { intArray :: Array Int
  , optionalInt :: NullOrUndefined Int
  }
derive instance repGenericRecordWithArrayAndNullOrUndefined :: Generic RecordWithArrayAndNullOrUndefined _
instance decodeRecordWithArrayAndNullOrUndefined :: Decode RecordWithArrayAndNullOrUndefined where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeRecordWithArrayAndNullOrUndefined :: Encode RecordWithArrayAndNullOrUndefined where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
```

```hs
describe "RecordWithArrayAndNullOrUndefined" do
  testJSON
    (RecordWithArrayAndNullOrUndefined { intArray: [1, 2, 3] , optionalInt: wrap $ Just 1 })
    """{ "intArray": [1, 2, 3] }"""
    (Right (RecordWithArrayAndNullOrUndefined { intArray: [1, 2, 3] , optionalInt: wrap Nothing }))
```

```hs
RecordWithArrayAndNullOrUndefined
  ✓︎ can be converted to JSON
    (RecordWithArrayAndNullOrUndefined { intArray: [1,2,3], optionalInt: (NullOrUndefined (Just 1)) })
    -> {"optionalInt":1,"intArray":[1,2,3]}
  ✓︎ can be converted back
  ✓︎ can be converted from JSON
    { "intArray": [1, 2, 3] }
    -> (Right (RecordWithArrayAndNullOrUndefined { intArray: [1,2,3], optionalInt: (NullOrUndefined Nothing) }))
```

We can see that the property will be written if defined, and the appropriate value will be returned for an undefined property.

## 以上

This is about it for the most common usages for Foreign-Generic. Exceptional cases can be handled by writing the instances either manually or by writing your own function for Generic (if you'd like to learn about Generic programming, you might be interested in my [previous post here](http://qiita.com/kimagure/items/cc0ea2982abdf1625e87)).

I hope this provides some examples that will come in handy for those of you using Foreign-Generic, and hopefully inspires those of you who are not yet users to check it out! There are also similar utilities available for Argonaut in [Argonaut-Generic](https://github.com/purescript-contrib/purescript-argonaut-generic), if you're a user there.

Even if you don't use Purescript, I hope this has piqued your interest in automatic JSON de/encoding using Generic programming.

## Links

* Howto-Foreign-Generic: https://github.com/justinwoo/purescript-howto-foreign-generic
* Foreign-Generic: https://github.com/paf31/purescript-foreign-generic/

