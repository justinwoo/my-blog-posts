Last time, I wrote about using Generics-Rep to be able to decode JSON into sum types using the Simple-JSON library to actually perform all the actual parsing of elements [here](https://qiita.com/kimagure/items/b27245a5a11462145bd5).

In that example, I didn't add any handling for product types. To be honest, any time you want to encode a product type, you should probably be using records instead. However, if you really want encoding of product types, then one way you might choose to encode them is to use a heterogeneous array.

## Review

Last time, we had this type class for doing generic sum decoding:

```hs
class ReadForeignGenericSum a where
  readForeignGenericSum :: Foreign -> F a
```

So to add a case for handling product types we'll add an instance for Product for this class. But to write that instance, we need to be able to somehow be able to correctly read a value from an index of an array. So, probably yet another type class.

## ReadForeignGenericProduct

So this time around, there's nothing more we need other than just to be able to use an offset. So we'll define our class like so:

```hs
class ReadForeignGenericProduct a where
  readForeignGenericProduct :: Int -> Foreign -> F a
```

Another good thing is that the Product reps are right-nested, and so a three-arg constructor will look like `Constructor name (Product (Argument a) (Product (Argument b) (Argument c)))`. So the offset of the left has to be the number, and we increment the number to handle all the right cases.

```hs
instance rfgpProduct ::
  ( ReadForeignGenericProduct a
  , ReadForeignGenericProduct b
  ) => ReadForeignGenericProduct (Product a b) where
  readForeignGenericProduct i f
        = Product
      <$> readForeignGenericProduct i f
      <*> readForeignGenericProduct (i + 1) f
```

Then for reading the arguments, we just use the offset provided.

```hs
instance rfgpArg ::
  ( ReadForeign a
  ) => ReadForeignGenericProduct (Argument a) where
  readForeignGenericProduct i f = do
    Argument <$> (read =<< readIndex i f)
```

With this, we're done defining our instances for this class. Then we can write the instance for readForeignGenericSum, where we start with an initial offset of 0.

```hs
instance rfgsProduct ::
  ( ReadForeignGenericProduct (Product a b)
  ) => ReadForeignGenericSum (Product a b) where
  readForeignGenericSum f =
    readForeignGenericProduct 0 =<< readProp "value" f
```

And that's it!

## Usage

Now we can take the same test case we had before and just add one more constructor:

```diff
 data Fruit
   = Apple
   | Grapes Int
+  | Bananas String String Int
   | Thing { name :: String, count :: Int, color :: String }
```

And with no other changes, our original test case will continue to work, and we can add a new one:

```hs
    let
      testJSON2 = """
      {
        "type": "Bananas",
        "value": ["Green", "Big", 3]
      }
      """

      b :: Either (NonEmptyList ForeignError) Fruit
      b = readJSON testJSON2

    pending $ show b
    -- (Right (Bananas "Green" "Big" 3))

    it "works with product types" do
      isRight b `shouldEqual` true
```

## Conclusion

So I hope these two posts have shown you some basics on how to use datatype generics and what all you can do to solve problems you might have, like this generic JSON serialization problem.

## Links

* This repo: https://github.com/justinwoo/simple-json-generic-sums