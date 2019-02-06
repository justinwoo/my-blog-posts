---
title: Generic Decoding of Sum Types Feat. Fields to Row and vice versa
tags: purescript
author: kimagure
slide: false
---
## Update

As of PureScript 0.12, Generics-Rep does not derive or use `Record` and `Field` for records. So a lot of the hacks here for how to work with Record types can be safely ignored now, but you might find them useful as more demonstrations of how to do some type-level programming to get types you want.

-----

Something that people have asked me about for a long time is how to de/encode sum types using Simple-JSON. While I'm a strong believer that there's almost no such thing as encoding actual sum types in JSON (you can only encode a polymorphic variant at best), it can still be worthwhile to parse into a sum type to prevent having to deal with variants all over your codebase.

So in this post, I'll go over how one might choose to implement sum type decoding using Generics-Rep (aka Datatype Generics, GHC-style Generics), so that they don't have to write concrete decoding instances for each sum type they have.

*現在状況 - 2018 Feb*

*Currently, in PureScript 0.11.7, we have Generics-Rep derived for records even though this is now made basically obsolete by RowToList. This post will go over some techniques of converting these Rep structures into row types in the style of [this post](https://qiita.com/kimagure/items/f750d85377520a14066f). However, if you stumble on this post at a later point, hopefully this will just be a source of amusement.*

## Goals

Similar to the [ReadForeign](https://pursuit.purescript.org/packages/purescript-simple-json/docs/Simple.JSON#t:ReadForeign) class, we will need a way to parse from Foreign. The difference here that we will parse into a generic representation of our data type which can be converted by Generic to and from our concrete data type:

```hs
class ReadForeignGenericSum a where
  readForeignGenericSum :: Foreign -> F a
```

Then our main exposed functions will take in the desired type from the context, apply a constraint for the type having a [derived] instance of Generic and having an instance of ReadForeignGenericSum the Generic-Rep:

```hs
genericReadForeignGenericSum :: forall a rep
   . Generic a rep
  => ReadForeignGenericSum rep
  => Foreign
  -> F a
genericReadForeignGenericSum f = to <$> readForeignGenericSum f

genericReadForeignGenericSumJSON :: forall a rep
   . Generic a rep
  => ReadForeignGenericSum rep
  => String
  -> F a
genericReadForeignGenericSumJSON s = genericReadForeignGenericSum =<< parseJSON s
```

## ReadForeignGenericSum instances of Reps

As we're handling sum types only, there are only a few cases we actually need to handle:

* Decoding the sums
* Decoding a constructor and its inner item
* Decoding no arguments (when the constructor is nullary)
* Decoding a single argument (when the constructor has an argument that is a concrete type)
* Decoding a record argument (the Generic-Rep of a record in 0.11.7)

### Decoding the Sum Rep

As we're looking to find whichever member of our sum type parses correctly, we'll be using the [Alt](https://pursuit.purescript.org/packages/purescript-control/docs/Control.Alt#t:Alt) instance of `F` (`Except ForeignErrors`) to try out different parsers. To produce a value rep of sum, we need to produce a value on the left or right side using the [Inl/Inr](https://pursuit.purescript.org/packages/purescript-generics-rep/docs/Data.Generic.Rep#t:Sum) constructors. So, put into code:

```hs
instance rfgsSum ::
  ( ReadForeignGenericSum a
  , ReadForeignGenericSum b
  ) => ReadForeignGenericSum (Sum a b) where
  readForeignGenericSum f
      = Inl <$> readForeignGenericSum f
    <|> Inr <$> readForeignGenericSum f
```

### Decoding the Constructor Rep

To match our constructor, I have chosen to use a string stored in a "type" field in the JSON. After reading this property from our Foreign object as a string, I check if the string matches the name of the constructor. If the name matches, I continue on and read the inner item. If not, I throw an error that the tag did not match.

```hs
instance rfgsCons ::
  ( IsSymbol name
  , ReadForeignGenericSum a
  ) => ReadForeignGenericSum (Constructor name a) where
  readForeignGenericSum f = do
    ty <- readString =<< readProp "type" f
    if name == ty
      then
        Constructor <$> readForeignGenericSum f
      else
        throwError <<< pure <<< ForeignError
          $ "could not match given " <> ty <> " with " <> name
    where
      name = reflectSymbol (SProxy :: SProxy name)
```

### Decoding the NoArguments Rep

In this case, there is nothing really to do. Our instance quite literally just returns the success case.

```hs
instance rfgsNoArg :: ReadForeignGenericSum NoArguments where
  readForeignGenericSum _ =
    pure NoArguments
```

### Decoding the Argument Rep

For the single argument, I've decided to shove the value in a "value" field. I then have my instance so that the property is read, then `read` from Simple-JSON is applied with the ReadForeign constraint to the result. When the operation has succeeded, then I can apply my Argument constructor to get the Rep:

```hs
instance rfgsArg ::
  ( ReadForeign a
  ) => ReadForeignGenericSum (Argument a) where
  readForeignGenericSum f =
    Argument <$> (read =<< readProp "value" f)
```

### Decoding the Record Rep

While this will not be in Generics-Rep in the future, as of 0.11.7 we still need to deal with this. Essentially, what we would like to work with is `Record row`/`{ | row}`, but the Rep we have to work with is `Rec (Product (Field name1 ty1) (Product (Field name2 ty2) ...))`. To reuse the Record ReadForeign instance, we need to some type-level conversion work.

## Converting a Record Rep fields to row

We can define a typeclass that will only apply the type-level transformations that we need like so:

```hs
class FieldsToRow fields (row :: # Type)
```

And in the case of our fields, we have two cases to handle: the product and field cases. The product case is much like the sum case, but where we combine the result of our operations in a Union:

```hs
instance productFieldsToRow ::
  ( FieldsToRow a l
  , FieldsToRow b r
  , Union l r row
  ) => FieldsToRow (Product a b) row
```

Then for our field case, we can use RowCons to build the unary rows of the field name and type:

```hs
instance fieldFieldsToRow ::
  ( RowCons name ty () row
  ) => FieldsToRow (Field name ty) row
```

And that's all we need to convert the fields rep to a row type, so that we can now read our JSON to the correct record type. However, with the limitation that we need to create values of our Rep, we need to unfortunately turn this back into a value of Fields. How? Well, now we can write the reverse using our concrete record.

## Converting a record to fields Rep values

Similar to last time, we'll have a type class with fields and row as parameters. However, this time we'll take in a record and output values of our fields:

```hs
class RecordToFields fields (row :: # Type) where
  recordToFields :: { | row } -> fields
```

In this case, our product will similarly combine the results but only on the value level. This is because I will pass in my entire record for fields to be read from, rather than to delete fields wastefully:

```hs
instance rtfProduct ::
  ( RecordToFields a row
  , RecordToFields b row
  ) => RecordToFields (Product a b) row where
  recordToFields r =
    Product (recordToFields r) (recordToFields r)
```

Finally, we can write individual fields with the constraint that our field of name and type exist in the total row, using `get` from [Record](https://pursuit.purescript.org/packages/purescript-record/docs/Data.Record#v:get).

With this, we can write our instance of ReadForeignGenericSum for Rec:

```hs
instance rfgsRec ::
  ( FieldsToRow fields row
  , RecordToFields fields row
  , ReadForeign (Record row)
  ) => ReadForeignGenericSum (Rec fields) where
  readForeignGenericSum f = do
    value :: Record row <- read =<< readProp "value" f
    pure <<< Rec $ recordToFields value
```

## Usage

After all that, which didn't amount to too much code, thankfully, we can put this to use. And so, with the normal Generic-deriving mechanisms:

```hs
data Fruit
  = Apple
  | Grapes Int
  | Thing { name :: String, count :: Int, color :: String }
derive instance gFruit :: Generic Fruit _
instance sFruit :: Show Fruit where
  show = genericShow
instance rfFruit :: ReadForeign Fruit where
  readImpl = genericReadForeignGenericSum
```

And our usage we can put in a spec and run:

```hs
main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "genericReadForeignGenericSumJSON" do
    let
      testJSON1 :: String
      testJSON1 = """
      {
        "type": "Thing",
        "value": { "name": "watermelon", "count": 1, "color": "purple" }
      }
      """

      a :: Either (NonEmptyList ForeignError) Fruit
      a = readJSON testJSON1

    pending $ show a
    -- (Right (Thing { color: "purple", count: 1, name: "watermelon" }))

    it "works" do
      isRight a `shouldEqual` true
```

Which outputs this when we run `pulp test`:

```
genericReadForeignGenericSumJSON
  ~ (Right (Thing { color: "purple", count: 1, name: "watermelon" }))
  ✓︎ works
```

And that's it!

## Conclusion

So while overall this does have some undesirable characteristics (as of 0.11.7), it can still be overall useful when you know this isn't going to be a limiting factor in your programs (I guess realistically it might never be). Hopefully this gives you some ideas about how to do this, or you might fork this out and work with your own version.

If you think this would be useful in the main library or published as a library itself as "utils" or something, please open an issue on GitHub. I guess if others really want it, we could add it in.

## Link

* This repo: https://github.com/justinwoo/simple-json-generic-sums

