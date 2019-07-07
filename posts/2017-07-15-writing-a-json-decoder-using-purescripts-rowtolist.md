# Writing a JSON decoder using Purescript's RowToList

*Update: with the release of Purescript-Record recently, there's no longer a need to use some of the hacks described in the implementation at the very end. See https://twitter.com/jusrin00/status/888096990088888321 and the follow up article here: http://qiita.com/kimagure/items/7d777826acf371293a93*

With the recent Purescript 0.11.6 release, we can now turn row types into type-level lists. There's a nice post by Liam [here](https://liamgoodacre.github.io/purescript/rows/records/2017/07/10/purescript-row-to-list.html) that expains how these work and provides an involved example, but this one will just be a light overview going into a much simpler usage that involves going through a single type-level list.

With this, I made [Simple-JSON](https://pursuit.purescript.org/packages/purescript-simple-json), which lets you decode JSON with nothing more than a **type alias**. No setup required!

Let's dig into the details.

## Why use RowToList?

Typically, the way one parses JSON in Purescript is to use something like Foreign-Generics as I described [in a previous post](http://qiita.com/kimagure/items/00f97c7fc6cef178fa3c), but this has a couple problems:

1. You can't use the record type directly, so you have to define a newtype for the record you want to parse.
2. You need `Encode`/`Decode` instances, so you need some way to get these -- typically using a `Generic` instance and using `genericEncode/Decode`.
3. You need a `Generic` instance to use these

While you can accomplish this with one newtype definition, a line to derive `Generic`, and two lines to define serialization instances in terms of generics, this is still missing the point -- you just need a simple record type parsed, none of this extra ceremony.

This is why a RowToList approach is super nice, since you don't need this ceremony.

## What is a Record even?

A record in Purescript is a type that takes a row type to produce a type:

```hs
data Record :: # Type -> Type
```

`# Type` is the row type that we're interested in, which we will be using to convert to a list. Consider the familiar form:

```hs
type MyRecord t = { a :: Int, b :: String | t}
```

This is a record with fields of "a" Int and "b" String, which can be extended with any row type `t`. This is sugar for this form:

```hs
type MyRecord t = Record ( a :: Int, b :: String | t)
```

If this looks familiar, it's because you've probably already been using it -- in Eff rows! *You may also remember these from my earlier post talking about [tracking validations with row types](http://qiita.com/kimagure/items/5c248844ab28c8c91b16).*

What `RowToList` allows us to do is to take this `# Type` row type and convert it into a `RowList`, which is a cons-list made of `Cons (name :: Symbol) (ty :: Type) (tail :: RowList)` and `Nil`. With this knowledge, we can get to work.

## Some initial setup

Let's make some initial preparations first -- we want to do operations on a `Foreign` object, and use a helper defined in Foreign-Generic for the actual JSON operations. And since parsing operations can fail, we'll reuse the type alias `F a` for `Except MultipleErrors a` defined in Foreign-Generic.

```hs
readJSON :: forall a
  .  ReadForeign a
  => String
  -> F a
readJSON = readImpl <=< parseJSON
```

Next, we define the type class `ReadForeign` and some instances:

```hs
class ReadForeign a where
  readImpl :: Foreign -> F a

instance readForeign :: ReadForeign Foreign where
  readImpl = pure

instance readChar :: ReadForeign Char where
  readImpl = readChar

instance readNumber :: ReadForeign Number where
  readImpl = readNumber

instance readInt :: ReadForeign Int where
  readImpl = readInt

instance readString :: ReadForeign String where
  readImpl = readString

instance readBoolean :: ReadForeign Boolean where
  readImpl = readBoolean

instance readArray :: ReadForeign a => ReadForeign (Array a) where
  readImpl = readElements <=< readArray
    where
      readElements xs = sequence $ readImpl <$> xs
```

So far all normal. So let's get into our record instance.

## RowToList

For records, we know that we will be working with an instance for `Record fields`, where `fields :: # Type`, which we can turn into a `fieldList :: RowList` with `RowToList`. With this knowledge, we can write the following instance:

```hs
instance readRecord ::
  ( RowToList fields fieldList
  , ReadForeignFields fieldList
  , ListToRow fieldList fields
  ) => ReadForeign (Record fields) where
  -- [...]
```

We have the complementary `ListToRow` here because the ordering of keys is not necessarily the same, but this lets us lock in `field` and `fieldList`.

Let's look at `ReadForeignFields` for how we work with the `RowList` we've gotten.

```hs
class ReadForeignFields (xs :: RowList) where
  getFields :: forall fields
    .  RowToList fields xs
    => ListToRow xs fields
    => Proxy (Record fields)
    -> Foreign
    -> F (StrMap Foreign)
```

While the class is for `RowList`, we don't have a `RLProxy (xs :: RowList)` (though we *could* define one), so we'll pass in a `Proxy` of the record instead and check the row and list like we did before.

While the `StrMap Foreign` looks strange, this is useful for building up Javascript objects as the underlying representation are objects (since JS objects can't really readily use anything other than string keys).

With this knowledge, we can write the instance for the `Cons` case:

```hs
instance readFieldsCons ::
  ( IsSymbol name
  , ReadForeign ty
  , ListToRow tail tailRow
  , ReadForeignFields tail
  , RowToList tailRow tail
  ) => ReadForeignFields (Cons name ty tail) where
  getFields _ obj = do
    field <- readProp name obj
    first :: ty <- readImpl field
    rest <- getFields (Proxy :: Proxy (Record tailRow)) obj
    pure $ union (singleton name field) rest
    where
      name = reflectSymbol (SProxy :: SProxy name)
```

Here we take the `Symbol` name and use it to read the field we want, checking its contents by parsing against the type that it is supposed to be in our record. We take the rest by passing along the tail and union the results.

The base case `Nil` ends up being quite simple:

```hs
instance readFieldsNil :: ReadForeignFields Nil where
  getFields _ _ = pure empty
```

Since we don't use any information, we can simply return the empty `StrMap` here.

## 出来上がり～

With just this much "real code", we've finished our record alias Foreign/JSON decoder.

Here it is in action:

```hs
type E a = Either MultipleErrors a

handleJSON :: forall a. ReadForeign a => String -> E a
handleJSON json = runExcept $ readJSON json

type MyTest =
  { a :: Int
  , b :: String
  , c :: Boolean
  , d :: Array String
  }

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "readJSON" do
    it "works with proper JSON" do
      let result = handleJSON """
        { "a": 1, "b": "asdf", "c": true, "d": ["A", "B"]}
      """
    it "fails with invalid JSON" do
      let result = handleJSON """
        { "c": 1, "d": 2}
      """
      isRight (result :: E MyTest) `shouldEqual` false
```

By providing the type to be used in the usage of `result`, we're able to make the `handleJSON` (and in turn, `read` and `readImpl`) resolve the type to be used. No extra work involved!

Hopefully this has shown that `RowToList` is a really fun tool for being able to work with row types in powerful ways.

## Links

* repo: https://github.com/justinwoo/purescript-simple-json
* Liam's post on RowToList: https://liamgoodacre.github.io/purescript/rows/records/2017/07/10/purescript-row-to-list.html
* Reddit thread with some more things: https://www.reddit.com/r/purescript/comments/6mss5o/new_in_purescript_0116_rowtolist/

## Bonus

```hs
let result = handleJSON """
  [{"a": 1}, {"a": 2}, {"a": 3}]
"""
isRight (result :: E (Array {a :: Int})) `shouldEqual` true
```