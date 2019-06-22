Last week, I spoke at Monadic Party, a workshop-oriented conference in Poznan, Poland (<https://monadic.party/>). At the event, I happened to catch Arthur Xaiver's talk on type-safe DSLs, and started to wonder about a rather basic-ish problem of mine: building SQLite queries but with some typed parameters.

I made this small example to go along with my workshop "Real World PureScript", which is an introduction to the PureScript type system and FFI, and how types can be used to provide nicer and stronger guarantees on FFI: <https://github.com/justinwoo/real-world-purescript-workshop-2019>

## What do I need?

Basically, when one works with the node sqlite3 library, they need to prepare a string query with parameters named "$parameter" and so on. These labels correspond to a record of these `$parameter`s, like so:

```js
query(
  'select * from world where id = $id',
  { "$id": 1 }
)
```

In some kind of way, I need to be able to build up a query that will hold this information. And it turns out, I already had this tool a long time ago.

## `Row.Union` solves everything

We can define a parameterized query as having a row type parameter which will match up to the record that will contain the required params, as so:

```purs
data ParamQuery (params :: # Type) = ParamQuery String
```

So if we want to join two query portions together, we simply only need to merge together the row type in the type level, and merge together the strings in the value level.

```purs
joinThings :: forall r1 r2 r3 r
   . Row.Union r1 r2 r3
  => Row.Nub r3 r
  => ParamQuery r1 -> ParamQuery r2 -> ParamQuery r
joinThings (ParamQuery s1) (ParamQuery s2) = ParamQuery $ s1 <> " " <> s2
```

One additional detail we use in the type level is to simply `Nub` off duplicate parameter fields, considering that the same parameter can be used in multiple places of a query.

And that's it. This is pretty much the only thing we need to make this work. For convenience, we also define an operator alias:

```purs
infixl 1 joinThings as <<>>
```

## Individual parameter sections

Building a literal section is fairly easy:

```purs
literal :: String -> ParamQuery ()
literal = ParamQuery
```

However, to build a parameter name section, we need to make sure the parameter name is prefixed with `$` as above, and we need to use the actual field label for the parameter string. This is made simple enough by using `reflectSymbol` on a `Symbol` parameter.

```purs
data Param (name :: Symbol) ty = Param

param :: forall name ty r
   . Row.Cons name ty () r
  => IsSymbol name
  => Param name ty -> ParamQuery r
param _ = ParamQuery name
  where
    nameS = SProxy :: SProxy name
    -- parameter labels must be renamed for use with node-sqlite3
    name = "$" <> reflectSymbol nameS
```

We use the `Row.Cons` here to add a generic `name`, `ty` field to an empty row to create a singleton row, which is then transferred onwards.

## Running the query with the parameters

Then all I need is to define a function that will use the `ParamQuery`, remembering to rename the input record fields to prepend the `$`.

```purs
foreign import renameParamLabels :: forall r1 r2. { | r1 } -> { | r2 }

queryDB
  :: forall params
   . SQLite3.DBConnection
  -> ParamQuery params
  -> { | params }
  -> Aff Foreign
queryDB db (ParamQuery query) params_ =
  SQLite3.queryObjectDB db query (renameParamLabels params_)
```

And so, this will do everything we need.

## Usage

Once we have this setup, we can very easily put this to use:

```purs
    insert
         = B.literal "insert into mytable values ("
      <<>> B.param (B.Param :: _ "name" String)
      <<>> B.literal ","
      <<>> B.param (B.Param :: _ "count" Int)
      <<>> B.literal ");"

    getByName
         = B.literal "select * from mytable where name ="
      <<>> B.param (B.Param :: _ "name" String)
      <<>> B.literal "and name ="
      <<>> B.param (B.Param :: _ "name" String)
```

We can also look at the inferred type of these queries if we use `insert :: ?hole` and such:

```purs
  Hole 'sdfd' has the inferred type

    ParamQuery
      ( count :: Int
      , name :: String
      )
```

```purs
  Hole 'sdf' has the inferred type

    ParamQuery
      ( name :: String
      )
```

Then we can have a test that sets up a database and runs the queries:

```purs
main :: Effect Unit
main = launchAff_ do
  db <- SL.newDB "./test/testdb.sqlite"
  _ <- SL.queryDB db "create table if not exists mytable (name text, count int)" []
  _ <- SL.queryDB db "delete from mytable" []

  _ <- B.queryDB db insert { name: "apples", count: 3 }
  _ <- B.queryDB db insert { name: "bananas", count: 6 }

  f1 <- B.queryDB db getByName { name: "apples" }
  testResult f1 [{ name: "apples", count: 3 }]

  f2 <- B.queryDB db getByName { name: "bananas" }
  testResult f2 [{ name: "bananas", count: 6 }]

  log "tests passed"
  where
    testResult f expected =
      case JSON.read f of
        Left e -> throwError (error $ show e)
        Right (actual :: Array { name :: String, count :: Int }) ->
          assertEqual { actual, expected }
    assertEqual = liftEffect <<< Assert.assertEqual
```

Awesome.

## Conclusion

While this isn't the most fancy example, I hope this gives you some ideas on simple ways you could get types to do more of your bidding.

I wrote this while stuck in Frankfurt because rain caused my flight to be delayed by an hour.

## Links

* This repo <https://github.com/justinwoo/purescript-bingsu/>

* Real World PureScript <https://github.com/justinwoo/real-world-purescript-workshop-2019>
