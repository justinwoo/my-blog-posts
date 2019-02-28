## 前回のあらすじ

Last time, I wrote ["Well-typed path params in PureScript 0.12"](https://qiita.com/kimagure/items/3273d20c4c5ad74dbe26), in which I talked about how I used the [Record-Format](https://github.com/kcsongor/purescript-record-format) library Csongor Kiss made to then make a `Symbol` (type-level string) templated URL to Record parsing library that allows us to use the template to make a function of `String -> Either _ { | fields }`, where `fields` is extracted from parameters in the template:

```hs
main = do
  let (parseURL'
        -- inferred type:
        :: String -> Either String { name :: String, age :: String })
        = parseURL (SProxy :: SProxy "/hello/{name}/{age}")
  let parsed = parseURL' "/hello/Bill/12"
  case parsed of
    Right r -> do
      assert $ r.name == "Bill"
      assert $ r.age == "12"
    Left e -> ...
```

While this works very well for homogeneous records of `String`, our solution last time for heterogeneous records was to make a `convertRecord` function to read the different fields here. While this works if you provide a concrete type in the context, I wanted to push the type information back into the template, like so:

```hs
main = do
  let (parseURL2'
        -- inferred type:
        :: String -> Either String { name :: String, age :: Int })
        = parseURL (SProxy :: SProxy "/hello/{name:String}/{age:Int}")
```

Well, this works! But let's dig into how it works.

## Revisiting ParseURLImpl

The heavy lifting of this library happens in the `ParseURLImpl` type class defined as

```hs
class ParseURLImpl (xs :: FList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  parseURLImpl
    :: FProxy xs
    -> String
    -> Either String (Builder { | from } { | to })
```

Where the `xs :: FList` is the formatting information from before, and the `from` and `to` rows are for returning our `Record.Builder`. And in the `FCons (Var name)` case for our formatting parameter, we have the constraints:

```hs
instance consVarParseURLImpl ::
  ( IsSymbol name
  , Row.Cons name String from' to
  , Row.Lacks name from'
  , ParseURLImpl tail from from'
  ) => ParseURLImpl (FCons (Var name) tail) from to where
```

So we can see here that the `Row.Cons` for the builder uses the whole `name` parameter and sets `String` as its type, e.g. given `"/hello/{name}/{age}"`, we would get a builder for `( name :: String, age :: String )`. So if we wanted to add optional type annotations for the parameters, we would have to use a type class that would parse out the name and type we want to use from the supplied `Symbol`:

```diff
instance consVarParseURLImpl ::
- ( IsSymbol name
- , Row.Cons name String from' to
+ ( ParseTypedParam s name ty
+ , ReadParam ty
+ , IsSymbol name
+ , Row.Cons name ty from' to
  , Row.Lacks name from'
  , ParseURLImpl tail from from'
- ) => ParseURLImpl (FCons (Var name) tail) from to where
+ ) => ParseURLImpl (FCons (Var s) tail) from to where
```

So we use this `ParseTypedParam` type class with the symbol of `Var` to get back `name` and `ty`, and apply the `ReadParam ty` constraint to be able to read the string value to `Either _ ty`.

Accordingly, we need to change the body of the instance to use `readParam` on the `String` value to the type result:

```diff
  parseURLImpl _ s = do
    split' <- split
-   let first = Builder.insert nameP split'.before
+   value <- readParam split'.before
+   let first = Builder.insert nameP value
    rest <- parseURLImpl (FProxy :: FProxy tail) split'.after
    pure $ first <<< rest
```

## `ParseTypedParam`

The class definition is practically a stub that calls the implementation:

```hs
class ParseTypedParam (s :: Symbol) (name :: Symbol) (ty :: Type) | s -> name ty
instance parseTypedParam ::
  ( Symbol.Cons x xs s
  , ParseTypedParamImpl x xs "" name ty
  ) => ParseTypedParam s name ty
```

`Symbol.Cons` is a new compiler-solved type class which can be used to deconstruct a given `Symbol` into its head and tail:

```hs
class Cons (head :: Symbol) (tail :: Symbol) (symbol :: Symbol)
  | head tail -> symbol, symbol -> head tail
```

The two fundeps are very useful here, as they allow us to perform not only deconstruction but construction of a `Symbol` from its parts, though `Append` is more easily applicable:

```hs
class Append (left :: Symbol) (right :: Symbol) (appended :: Symbol)
  | left right -> appended, right appended -> left, appended left -> right
```

## `ParseTypedParamImpl`

This class contains the actual implementation by building up an accumulating `Symbol` for the actual name we want to return along with the type that the parameter should have:

```hs
class ParseTypedParamImpl
  (x :: Symbol) (xs :: Symbol) (acc :: Symbol)
  (name :: Symbol) (ty :: Type)
  | x xs acc -> name ty
```

### No type annotation

First, we should handle the case in which the whole string has been parsed without running into a type annotation, where we will return the total accumulated `Symbol` with the default `String` type:

```hs
instance noMatchTypedParamImpl ::
  ( Symbol.Append acc x name
  ) => ParseTypedParamImpl x "" acc name String
```

We match the end of the string has been reached by matching on an empty string for the tail of the `Symbol.Cons` deconstructed `Symbol`.

### With type annotation

Then in the case that we run into a colon, we assume that the rest of the string is the type name, so we feed that into a `MatchTypeName` class that we'll define to get back the type that we want to return:

```hs
else instance colonSplitParseTypedParamImpl ::
  ( MatchTypeName tyName ty
  ) => ParseTypedParamImpl ":" tyName name name ty

class MatchTypeName (s :: Symbol) (ty :: Type) | s -> ty
instance stringParamTypeSymbol :: MatchTypeName "String" String
else instance intParamTypeSymbol :: MatchTypeName "Int" Int
else instance errParamTypeSymbol ::
  ( Symbol.Append "Can't match type annotation to type: " s msg
  , TE.Fail (TE.Text msg)
  ) => MatchTypeName s ty
```

And in the case that the type annotation isn't matched, we give back the custom error message with the `Symbol` like so:

```
Error found:
in module Test.Main
at test/Main.purs line 32, column 11 - line 32, column 72

  A custom type error occurred while solving type class constraints:

    Can't match type annotation to type: Inta


while applying a function parseURL
  of type ParseURL t0 t1 => SProxy t0 -> String -> Either String { | t1 }
  to argument SProxy
while inferring the type of parseURL SProxy
in value declaration main
```

### Base case

For the base case where we neither have the end of the string or the type annotation, we should continue to apply the constraint with the rest of the string, while accumulating the name `Symbol`:

```hs
else instance baseParseTypedParamImpl ::
  ( Symbol.Cons y ys xs
  , Symbol.Append acc x acc'
  , ParseTypedParamImpl y ys acc' name ty
  ) => ParseTypedParamImpl x xs acc name ty
```

And that's all! We get to remove all the old `convertRecord` related code and keep the `ReadParam` code as-is.

## Usage

We can leave the first usage unchanged and the inferred type will remain the same:

```hs
  -- let (parseURL'
  --       -- inferred type:
  --       :: String -> Either String { name :: String, age :: String })
  let parseURL'
        = parseURL (SProxy :: SProxy "/hello/{name}/{age}")
  let parsed = parseURL' "/hello/Bill/12"
  case parsed of
    Left e -> do
      log $ "didn't work: " <> e
      assert $ 1 == 2
    Right r -> do
      assert $ r.name == "Bill"
      assert $ r.age == "12"
```

In the second usage, we can now remove the type annotations altogether by adding the type annotations to the template:

```hs
  -- let (parseURL2'
  --       -- inferred type:
  --       :: String -> Either String { name :: String, age :: Int })
  let parseURL2'
        = parseURL (SProxy :: SProxy "/hello/{name:String}/{age:Int}")
  let parsed2 = parseURL2' "/hello/Bill/12"
  case parsed2 of
    Left e -> do
      log $ "didn't work: " <> e
      assert $ 1 == 2
    Right r -> do
      assert $ r.name == "Bill"
      assert $ r.age == 12
```

And so without any more conversion work, we can get the result directly as a heterogeneous record with the fields being of the type of the annotations.

## Conclusion

Hopefully this has shown you how type-level string parsing in PureScript 0.12 gives you a lot of power for not too much work by giving you the ability to construct and deconstruct `Symbol`s with `Symbol.Cons` and chain overlapping instances with instance chains.

## Links

* This library: <https://github.com/justinwoo/purescript-kushiyaki>
* Csongor's post on well-typed printf in PureScript 0.12: <http://kcsongor.github.io/purescript-safe-printf/>