# Modified JSON parsing for free with PureScript-Simple-JSON

Last year, I made the Simple-JSON library using the PureScript RowToList feature, which allows you to parse from JSON to any record type alias. However, thanks to how typeclass constraints and type inference work in PureScript, you can parse from an unspecified type, given that you apply "concrete" transformations to a defined record type. These are made possible by two typeclasses provided in [Prim](https://pursuit.purescript.org/builtins/docs/Prim#t:Union), Union and RowCons, and the [PureScript-Record](https://pursuit.purescript.org/packages/purescript-record) library.

In this post I'll go into some of the examples I've made for PureScript-Record that demonstrate how transforming records with both the built-in record update syntax and PureScript-Record operations gives us parsing for free, even for types that don't have instances for the Simple-JSON ReadForeign type class.

## The big idea

If you've been through some kind of school system, you probably have learned how to do algebra. Given this equation:

```
a + b + c = d
```

You can solve for the fourth given any three variables. For example,

```
a = 1
b = 2
c = 3
d = ???
```

And the same for the fourth with any other three.

```
a = 1
b = ???
c = 3
d = 6
```

If you can do this, plugging in type class parameters isn't that much of a stretch. So in the case of adding a field to a sub-row,

```
RowCons name ty sub row
( name :: ty | sub ) ~ row
```

Since we almost always will have name and ty (type) available, the only remaining thing is if we have either sub or row. And if we provide one, we can solve for the other. So you can imagine what should happen here:

```
RowCons "apple" String sub (banana :: String)
sub ~ (apple :: String, banana :: String)
```

We'll look at how this comes to life in our examples.

## Changing a field

[Repo](https://github.com/justinwoo/parse-you-a-thingy-with-imperfect-json-that-you-convert-bad-properties-from/blob/master/src/Main.purs)

One usage that came up is that someone wanted to work with arrays where the JSON would be a nullable field. But instead of letting the nullable/Maybe type spread through their whole program, they wanted to contain this into where the parsing happened.

```hs
type MyThingy =
  { a :: String
  , b :: Array String
  }
```

From this, we then want an operation to modify the `b` field, such that this is the result, and the input is `Nullable (Array String)`. We can accomplish this with Data.Record.modify:

```hs
modify
  :: forall r1 r2 r l a b
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l -- Proxy for the Symbol l
  -> (a -> b) -- function for transforming a to b
  -> Record r1 -- input row
  -> Record r2 -- output row
```

So if we plug in `Nullable (Array String)` for `a` and `Array String` for `b`, we can get out the output record with the field with label `l` having `Array String`. Our parse function then looks like this:

```hs
parseMyThingyJsonFromImperfectJsonButConvertTheDirtyProperty ::
  String -> Either (NonEmptyList ForeignError) MyThingy
parseMyThingyJsonFromImperfectJsonButConvertTheDirtyProperty str =
  modify (SProxy :: SProxy "b") (fromMaybe [] <<< Nullable.toMaybe) <$> readJSON str
```

And better yet, since this is built-in as record update syntax and we know what label we want to use, we can just use that:

```hs
parseMyThingyJsonFromImperfectJsonButConvertTheDirtyProperty ::
  String -> Either (NonEmptyList ForeignError) MyThingy
parseMyThingyJsonFromImperfectJsonButConvertTheDirtyProperty str = do
  json <- readJSON str
  let b = fromMaybe [] <<< Nullable.toMaybe $ json.b
  pure $ json { b = b }
```

So with this, the type that will be used to parse the JSON will be `{ a :: String, b :: Nullable (Array String)}`, while the output of this function will correctly be MyThingy.

## Parsing a date from a string

[Repo](https://github.com/justinwoo/formatters-date-parsing-simple-json-example)

Parsing a date is something that usually turns out to be quite awful in JS. If you wrap moment.js, you will have to deal with the consequences of using their stringly-typed API and have various problems with 100% failure case code. If you use JSDate, it will be impossible to parse dates without your locale affecting the output, making using `new Date` and other JSDate methods impure and prone to errors.

Luckily, the [PureScript-Formatters](https://github.com/slamdata/purescript-formatters) library makes formatting dates purely quite easy, and gives you a pure [DateTime](https://pursuit.purescript.org/packages/purescript-datetime/3.4.1/docs/Data.DateTime#t:DateTime) type. This problem then becomes exactly the same as the previous:

```hs
type MyThing =
  { dateTime :: DateTime
  }

myDateFormat :: Formatter
myDateFormat
  = YearFull
  : Placeholder "/"
  : MonthTwoDigits
  : Placeholder "/"
  : DayOfMonthTwoDigits
  : Nil

parseMyThing :: String -> Either (NonEmptyList ForeignError) MyThing
parseMyThing s = do
  parsed <- readJSON s
  dateTime <- formatDateTime parsed.dateTime
  pure $ parsed { dateTime = dateTime }
  where
    formatDateTime = lmap (pure <<< ForeignError) <<< unformat myDateFormat
```

In this case, I take the error produced in the formatting operation and convert it into a ForeignError. So the formatDateTime function provides `String -> Either (NonEmptyList ForeignError) DateTime`, which makes the parsed JSON type `{ dateTime :: String }`. We don't have to deal with newtype wrapping dateTime to do this date parsing, and further get more choices on what format we want to parse from.

## Changing a field name

[Repo](https://github.com/justinwoo/change-field-name-simple-json-example)

Another common problem is that the names of the fields don't make much sense, or need to be renamed for whatever other reason. For this, we have a [rename](https://pursuit.purescript.org/packages/purescript-record/0.2.5/docs/Data.Record#v:rename) function in PureScript-Record. It has a pretty long type signature, but each part is fairly simple on its own:

```hs
rename :: forall prev next ty input inter output
   . IsSymbol prev -- previous name Symbol
  => IsSymbol next -- next name Symbol
  => RowCons prev ty inter input -- (prev, ty) + inter = input
  => RowLacks prev inter -- inter does not have a field with label prev
  => RowCons next ty inter output -- (next, ty) + inter = output
  => RowLacks next inter -- inter does not have a field with label next
  => SProxy prev
  -> SProxy next
  -> Record input
  -> Record output
```

[RowLacks](https://pursuit.purescript.org/packages/purescript-typelevel-prelude/2.5.0/docs/Type.Row#t:RowLacks) simply applies the constraints the the intermediate row type between renaming does not contain either the input or output-labeled field. This intermediate row being put into RowCons to form the output means that the output will not contain a input-labeled field, which is exactly what we want.

So in the case that we need to read the value for a "fieldA" from "MY_FIELD_A", our code looks something like this:

```hs
type MyThing =
  { "fieldA" :: String
  , "fieldB" :: Int
  }

decodeMyThingFromDirtyJSON :: String -> Either (NonEmptyList ForeignError) MyThing
decodeMyThingFromDirtyJSON s = do
  parsed <- readJSON s
  pure $ rename
    (SProxy :: SProxy "MY_FIELD_A")
    (SProxy :: SProxy "fieldA")
    parsed
```

And as your guessed, the type used by Simple-JSON to parse the incoming JSON contains `"MY_FIELD_A" :: String`. And if you need many fields renamed, there is a Builder API for Record, where Builder can be composed with other Builders as Builder is a Semigroupoid (like Function).

## Conclusion

Hopefully this has shown you that using generic record operations is fairly easy and worthwhile, and combined with Simple-JSON can give us free parsing for JSON by using the intermediate types available by context. While we could otherwise duplicate the type definition with the fields as what would be in our transported JSON, that would add the burden of having to change both the transport and desired type definitions -- which, while the types used for JSON decoding should not be much at all 1:1 in your actual logical types, this lets us not have to define manual decoders for every little thing. So for all the naysayers who talk about having to transform any automatically parsed types, this may be a complete solution to their problems.

*I originally meant to write this post last year, but forgot to actually write it. Oops.*

## Links

* Simple-JSON: https://github.com/justinwoo/purescript-simple-json
* PureScript-Record: https://pursuit.purescript.org/packages/purescript-record