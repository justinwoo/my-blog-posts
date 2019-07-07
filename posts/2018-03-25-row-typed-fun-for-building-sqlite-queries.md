# Row-typed fun for building SQLite Queries

Recently, I've been wanting to make some fairly naive SQLite queries using information from record types to map what inputs should go where and what type the output should be parsed to. Well, to work with the generic information of a record type, I pull out the RowToList tricks to get to work.

## Select All

The "simplest" of queries involves defining what record you want the results in, which means that we need two operations:

* A way to inline the keys of our record to be used in the select query: `select apple, banana, grape from table`
* A way to read out the result rows: is `{ apple: 123 }` -> Either Errors `{ apple :: Int}`

So really this means that we need 1) a keys implementation and 2) a kind of simple-json for these query results.

### Keys

If you're not familiar with RowToList and typeclass usage, you might check out these slides: <https://speakerdeck.com/justinwoo/rowlist-fun-with-purescript-2nd-edition>

So as in the slides, I implement keys by making a typeclass with a RowList parameter, whcih I can iterate as a Cons-list as usual, but at the type-level:

```hs
class Keys (xs :: RowList) where
  keys :: RLProxy xs -> List String
```

And so the Nil case is at the end of the list, so there are no more items:

```hs
instance nilKeys :: Keys Nil where
  keys _ = mempty
```

And for the Cons case, the Symbol `name` is reflected to the String value and this is constructed in a List with the rest of the results.

```hs
instance consKeys ::
  ( IsSymbol name
  , Keys tail
  ) => Keys (Cons name ty tail) where
  keys _ = List.Cons first rest
    where
      first = reflectSymbol (SProxy :: SProxy name)
      rest = keys (RLProxy :: RLProxy tail)
```

This is essentially how the rest of the classes work, but in more specific cases.

### Simple SQL Results

If you're unfamiliar with Record.Builder, you might be interested in reading more about it on my previous article that goes over a similar approach to Simple-JSON using Record.Builder: <https://qiita.com/kimagure/items/941c22effff608dda9a7>

Like Simple-JSON, I first define a class that works with the properties of the result object:

```hs
class FromResult a where
  fromResult :: Foreign -> F a
```

With `F` here being `Except MultipleErrors` from Data.Foreign.

I then have instances for the types that I'm interested in working with:

```hs
instance fromResultInt :: FromResult Int where
  fromResult = readInt

instance fromResultString :: FromResult String where
  fromResult = readString
```

Then, to build up a record as a result of parsing the individual fields, I use the Record.Builder API to parse out the fields and compose a function that can create my whole record from an initial record:

```hs
class FromResultFields (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  getFields :: RLProxy xs
    -> Foreign
    -> F (Builder (Record from) (Record to))
```

And since the instance only needs to be resolved by the RowList parameter, the functional dependency is set accordingly. The result is either some errors from the reading of the properties or the builder that will construct a record of my type `Record to` / `{ | to }`.

And as Builder is a semigroupoid like a regular function, the instances can be written in terms of the identity and composition of Builders:

```hs
instance fromResultFields :: FromResultFields Nil () () where
  getFields _ _ = pure id

instance fromResultFieldsCons ::
  ( IsSymbol name
  , FromResult ty
  , FromResultFields tail from from'
  , RowLacks name from'
  , RowCons name ty from' to
  ) => FromResultFields (Cons name ty tail) from to where
  getFields _ obj = do
    value <- withExcept' $ fromResult =<< readProp name obj
    compose (Builder.insert nameP value) <$> getFields tailP obj
    where
      nameP = SProxy :: SProxy name
      tailP = RLProxy :: RLProxy tail
      name = reflectSymbol nameP
      withExcept' = withExcept <<< map $ ErrorAtProperty name
```

Like before, the Nil case returns identity, while the cons instance does a lot of work here, but not much more than

1. Try to read the property at the given name from the input object
2. Try to read the property to the type specified
3. Make a Builder that will insert this value at the key
4. Get the `F` Builder of the rest of the fields and map into the Builder side of the `F` the composition with our current property's builder.

To put this class then to use, I then defined a function that uses this class and uses the builder on an empty object:

```hs
fromResultRow
  :: forall fields rl
   . RowToList fields rl
  => FromResultFields rl () fields
  => Foreign
  -> F { | fields }
fromResultRow o = buildRecord <$> fields
  where
    fields = getFields (RLProxy :: RLProxy rl) o
    buildRecord builder = Builder.build builder {}
```

### Definition

And with these classes defined, we can put these to work to define a function that uses the keys implementation to get the keys and reads the results.


```hs
selectAll
  :: forall result rl e
   . RowToList result rl
  => Keys rl
  => FromResultFields rl () result
  => Table
  -> SQL.DBConnection
  -> Aff (Effects e) (Array (F { | result }))
selectAll (Table table) db
    = map fromResultRow <<< asArray
  <$> SQL.queryDB db query mempty
  where
    columns = keys (RLProxy :: RLProxy rl)
    queryParts
      = "SELECT"
      : intercalate ", " columns
      : "FROM"
      : table
      : mempty
    query = intercalate " " queryParts <> ";"

    -- because i know the result is always an array from this query
    asArray f = unsafeCoerce f :: Array Foreign
```

And so the actual call to the SQLite library consists of nothing more than providing the DB connection, a string query, and an empty array of parameter arguments, but we have made the actual function call itself typesafe and more convenient to use.

## Insert or replace into

For the case of inserting/replacing entries, while we need the keys, we also need an incremented parameter index list and the actual parameters prepared in a list. To accomplish this, I defined a new class that can give me the column names, the argument index list, and the arguments themselves prepared for the SQLite library:

```hs
class PrepareInput (xs :: RowList) (row :: # Type)
  | xs -> row where
  prepareInputParams
    :: RLProxy xs
    -> { | row}
    -> Int
    -> { columns :: List String
       , params :: List String
       , args :: List SQL.Param
       }
```

Where the Int parameter is the index that I keep track of for sure in my functions.

And so, the Nil case is a record of empty things as expected:

```hs
instance prepareInputParamsNil :: PrepareInput Nil row where
  prepareInputParams _ _ _ = { columns: mempty, params: mempty, args: mempty }
```

For the next instance, I need to be able to convert the parameters to the strings that will be provided in the argument list. I define almost nothing more than what I had for FromResult:

```hs
class ToParam a where
  toParam :: a -> String

instance toParamInt :: ToParam Int where
  toParam = show

instance toParamString :: ToParam String where
  toParam = id
```

Then the Cons instance:

```hs
instance prepareInputParamsCons ::
  ( PrepareInput tail row
  , RowCons name ty trash row
  , IsSymbol name
  , ToParam ty
  ) => PrepareInput (Cons name ty tail) row where
  prepareInputParams _ r i = { columns, params, args }
    where
      nameP = SProxy :: SProxy name
      rest = prepareInputParams (RLProxy :: RLProxy tail) r (i + 1)
      columns = List.Cons (reflectSymbol nameP) rest.columns
      params = List.Cons ("$" <> show i) rest.params
      args = List.Cons (toParam $ get nameP r) rest.args
```

As before, we prepare the columns, params, and args in terms of the current.

### Definition

```hs
insertOrReplaceInto
  :: forall input inputL e
   . RowToList input inputL
  => PrepareInput inputL input
  => Table
  -> SQL.DBConnection
  -> { | input }
  -> Aff (Effects e) Unit
insertOrReplaceInto (Table table) db input =
  void $ SQL.queryDB db query (Array.fromFoldable params.args)
  where
    params = prepareInputParams (RLProxy :: RLProxy inputL) input 1
    queryParts
      = "INSERT OR REPLACE INTO"
      : table
      : ("(" <> intercalate ", " params.columns <> ")")
      : "VALUES"
      : ("(" <> intercalate ", " params.params <> ")")
      : mempty
    query = intercalate " " queryParts <> ";"
```

Such that if given some record `{ apple: 123 }`, the query created here would be `insert or replace into table (apple) values ($1)` and the args `["123"]`.

## Putting these to use

I also implemented "create table if not exists" and "delete from table where" queries in a similar way, and put these to use in a test suite:

```hs
main :: _
main = runTest do
  suite "Chanpon" do
    test "Works as expected" do

      -- delete the old DB if it exists
      _ <- attempt $ FS.unlink testDB
      
      -- make our new DB
      db <- newDB testDB

      C.createTableIfNotExists table db
        { name: "text primary key unique"
        , whatever: "text"
        }

      C.insertOrReplaceInto table db
        { name: "hello"
        , whatever: "world"
        }

      namesOnly <- sequence <$> C.selectAll table db
      withWhatever <- sequence <$> C.selectAll table db
      case runExcept $ Tuple <$> namesOnly <*> withWhatever of
        Right (Tuple left right)
          | [l :: {name :: String}] <- left
          , [r :: {name :: String, whatever :: String}] <- right -> do
            Assert.assert "expected value from names" $ l.name == "hello"
            Assert.assert "expected value from withWhatever" $ l.name == "hello"
            Assert.assert "expected value from withWhatever" $ r.whatever == "world"
        Right a -> do
          failure $ "incorrect results: " <> unsafeCoerce a
        Left e -> do
          failure $ show e

      C.deleteFrom table db { name: "hello" }

      result :: Array (F {name :: String}) <- C.selectAll table db
      Assert.assert "table is empty after deleting" $ Array.length result == 0
  where
    testDB = "./test/test.db"
    table = C.Table "test"
```

So you can see that the type of result of the `selectAll` queries are provided by context through annotations or usages, and the rest work through the inferred concrete record types from the values provided.

## Conclusion

Hopefully this has shown you that you can use typeclasses to make things easier for yourself. By minimizing the area of potential errors you can also make functions that are much easier to use than the individual operations. "The sum is greater than its parts", or something.

## Links

* This repo <https://github.com/justinwoo/purescript-chanpon/>
* Example usage in my vidtracker project <https://github.com/justinwoo/vidtracker/commit/c089415c1c835a7298f4debca675ae31166dfbfb>
* Slides about RowList and typeclass usage <https://speakerdeck.com/justinwoo/rowlist-fun-with-purescript-2nd-edition>