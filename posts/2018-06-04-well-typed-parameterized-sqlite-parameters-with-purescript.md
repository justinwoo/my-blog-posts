# Well-typed parameterized SQLite parameters with PureScript

When working with SQLite, the normal "type safe" way to use the library involves using a `String` value and a dictionary of `String` or `Foreign`/"any". This always ends up with the problem that even the most basic check of whether or not all of the query-specified parameters have been supplied is forced to happen in runtime, usually leading to lots of extra tests being written just to test this for every query written.

With PureScript 0.12, we can parse type level strings (`Symbol`) and use information from these to make a row type to require a record to be passed in, so that from a query string `"select * from table where name = $name and code = $code"`, we can get back a function of `Param a => Param b => { "$name" :: a, "$code" :: b } -> Connection -> Aff Result`.

## queryDB

At the top level, we want a function that will take in this query `Symbol`, apply a `ExtractParams` type class that we will define to get back `params :: # Type`, and use that extracted type with `Record`.

```hs
queryDB
  :: forall query params
   . IsSymbol query
  => ExtractParams query params
  => SQLite3.DBConnection
  -> SProxy query
  -> { | params }
  -> Aff Foreign
queryDB db queryP params =
  SQLite3.queryObjectDB db query params
  where
    query = reflectSymbol queryP
```

And that's actually about it, since we can use the underlying `node-sqlite3` library with the reflected value and our record of params.

## ExtractParams

From the above, it follows that our class has two parameters, where the query determines the params:

```hs
class ExtractParams
  (query :: Symbol) (params :: # Type)
  | query -> params
```

Then we know that to get started with parsing, we will need another class that actually uses its constraints to solve this out, where we first give it a start for parsing:

```hs
instance extractParams ::
  ( Symbol.Cons x xs query
  , ExtractParamsParse x xs params
  ) => ExtractParams query params
```

## ExtractParamsParse

Now we begin to get our hands dirty. Here we have a class where the head and tail determine the output row type:

```hs
class ExtractParamsParse
  (x :: Symbol) (xs :: Symbol) (params :: # Type)
  | x xs -> params
```

So we can start with the base case, where if the tail is empty, we can see that we no longer have anything to parse through, and the resulting row type here is an empty row:

```hs
instance endExtractParamsParse :: ExtractParamsParse x "" ()
```

The first concrete case we need will be handling `"$"`, which marks the beginning of a parameter name. In this case, we then need to...

1. split our tail into its own head and tail
1. use another defined class to parse out the name and start its parsed name accumulation with `"$"`
1. insert the parsed name with any allowed parameter type to our output row
1. continue the extraction of parameters for the subrow that we insert to

In code, this explanation becomes much clearer:

```hs
else instance parseParamExtractParams ::
  ( Symbol.Cons y ys xs
  , ParseParamName y ys "$" out
  , Symbol.Cons z zs ys
  , Row.Cons out ty row' row
  , AllowedParamType ty
  , ExtractParamsParse z zs row'
  ) => ExtractParamsParse "$" xs row
```

And this will be our first chained instance, as the first instance overlaps both on the head and the tail of this one.

Last is our least specific instance, which will just continue with the rest of the `Symbol`:

```hs
else instance nExtractParams ::
  ( Symbol.Cons y ys xs
  , ExtractParamsParse y ys row
  ) => ExtractParamsParse x xs row
```

## AllowedParamType

While this came up in our `"$"` instance earlier, there's not actually much to this. It simply declares that some types can be inserted in as params:

```hs
class AllowedParamType ty
instance stringAllowedParamType :: AllowedParamType String
instance intAllowedParamType :: AllowedParamType Int
instance numberAllowedParamType :: AllowedParamType Number
```

We'll see later how this gives us constrained type variables in inferred types later.

## ParseParamName

So this is the second big part of our library, where the actual parsing of the name is done until a delimiter/end is hit. The parameters we declared this should have earlier were the head, the tail, the accumulated `Symbol` so far, and the output `Symbol`.

```hs
class ParseParamName
  (x :: Symbol) (xs :: Symbol) (acc :: Symbol) (out :: Symbol)
  | x xs -> acc out
```

Again, we only match on the head and tail for instances, so the accumulate and output are determined by them in the instances.

The instances matching the end then handle the very normal cases, such as the sequence `")<end>"`, the last character, a space, a comma, and right parentheses with following characters.

```hs
instance endRParenParseParamName ::
  ParseParamName ")" "" out out

else instance endRParseParamName ::
  ( Symbol.Append acc x out
  ) => ParseParamName x "" acc out

else instance spaceParseParamName ::
  ParseParamName " " xs out out

else instance commaParseParamName ::
  ParseParamName "," xs out out

else instance parenParseParamName ::
  ParseParamName ")" xs out out
```

Of course, this may be an incomplete list of cases we want to handle, but for now these are the cases I want to handle.

Then, the general instance continues down the line splitting the tail while appending our current character to the accumulate:

```hs
else instance nParseParamName ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x acc'
  , ParseParamName y ys acc' out
  ) => ParseParamName x xs acc out
```

With this, we have our whole implementation.

## Usage

First, we can see how the query `"select name, count from mytable where name = $name and count = $count"` get inferred when fed to `queryDB`:

```hs
getEm
  :: forall a b
   . J.AllowedParamType a
  => J.AllowedParamType b
  => SL.DBConnection
  -> { "$name" :: a, "$count" :: b }
  -> Aff Foreign
getEm db = J.queryDB db queryP
  where
    queryP = SProxy :: SProxy "select name, count from mytable where name = $name and count = $count"
```

And just like that, we have our proper query with quantified variables for each field, where the field names have been extracted from our query. And with some concrete usage, everything will type check:

```hs
getSomethin :: SL.DBConnection -> Aff Foreign
getSomethin db = J.queryDB db queryP params
  where
    queryP = SProxy :: SProxy "select name, count from mytable where name = $name and count = $count"
    params = { "$name": "asdf", "$count": 4 }
```

And we need to be able to insert some items into our database, so let's add that:

```hs
addSomethin
  :: forall a b
   . J.AllowedParamType a
  => J.AllowedParamType b
  => SL.DBConnection
  -> { "$name" :: a, "$count" :: b }
  -> Aff Foreign
addSomethin db params = J.queryDB db queryP params
  where
    queryP = SProxy :: SProxy "insert or replace into mytable (name, count) values ($name, $count)"
```

Then our application of this becomes quite normal, where we just run whatever queries we need wherever:

```hs
type MyRow = { name :: String, count :: Int }

main :: Effect Unit
main = launchAff_ do
  db <- SL.newDB "./test/testdb.sqlite"
  _ <- SL.queryDB db "create table if not exists mytable (name text, count int)" []
  _ <- SL.queryDB db "delete from mytable" []
  _ <- addSomethin db { "$name": "apples", "$count": 3 }
  _ <- addSomethin db { "$name": "asdf", "$count": 4 }

  f1 <- getEm db { "$name": "apples", "$count": 3 }
  testResult f1 [{ name: "apples", count: 3 }]

  f2 <- getSomethin db
  testResult f2 [{ name: "asdf", count: 4 }]

  log "tests passed"
  where
    testResult f expected =
      case JSON.read f of
        Left e -> throwError (error $ show e)
        Right (actual :: Array MyRow) -> assertEqual { actual, expected }
    assertEqual = liftEffect <<< Assert.assertEqual
```

## Conclusion

I'm still really quite amazed by how this all worked out, as I had thought before learning PureScript that the answer to many problems was some form of codegen. Hopefully this has shown you how you can solve many of your problems in a first-class way in PureScript 0.12, and you'll try some of the methods I've described here and in my previous posts (especially my post about parsing type annotations [here](https://qiita.com/kimagure/items/6729a5d55ab99bcee8ec)).

## Links

* This repo <https://github.com/justinwoo/purescript-jajanmen>
* Previous post, "Parsing type-level strings to extract types" <https://qiita.com/kimagure/items/6729a5d55ab99bcee8ec>