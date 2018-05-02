# Well-typed path params in PureScript 0.12

Previously, I wrote about using tuples to represent URLs with parameters in my article [Type-level Path Params parsed to Records with PureScript](https://qiita.com/kimagure/items/4f5c6054870f631ff768). While the approach described in this article is fairly interesting, it's overall fairly disappointing to work with when the way to work with this looks like this:

```hs
type MyRoute = S "hello" / S "world" / Param "id" Int / Param "name" String
```

## Record-Format

With the release of PureScript 0.12-rc, we can revisit the work that Csongor did in his post [Well-typed printfs cannot go wrong](http://kcsongor.github.io/purescript-safe-printf/), where the combination of two major features allows for type-level parsing of Symbols:

* `Symbol.Cons`, which gives us the ability to constrain that a given `sym :: Symbol` can be split into `head :: Symbol` for the first "character" and `tail :: Symbol` for the remainder.
* Instance chains, which gives us the ability to define overlapping instances that can be resolved in defined order.

The post itself has a lot of details on how all this works, but for our usees, we will only need to use Csongor's `Record-Format` library, which defines two very useful kinds and a `Parse` type class:

```hs
foreign import kind Fmt -- ^ a format token is...
foreign import data Var :: Symbol -> Fmt -- ^ either a variable (to be replaced)
foreign import data Lit :: Symbol -> Fmt -- ^ or a literal

-- | A list of format tokens
foreign import kind FList
foreign import data FNil :: FList
foreign import data FCons :: Fmt -> FList -> FList

class Parse (i :: Symbol) (o :: FList) | i -> o
```

This type class works in that you provide a `Symbol` in the form `"hello {name}"`, and this will be parsed in the type level to produce a `FList` of `FCons (Lit "hello ") (FCons (Var "name") FNil)`.

## Change of Approach

So the original post I made, I had some key differences that forced me to work with things differerently:

* Because I was working with Tuple termini, I had to get both the builder of the subrow and the remaining string of Tuple branches, as the right branch will be run independently of the left.
* As there is no type information associated with the parameter in the `Symbol` parser, initially I will have to determine a homogeneous `String` record type and convert it later if I want to use different types. This doesn't give up much in the end though, as you'll see, as I do not have to provide contextual type information in this case.

## Kushiyaki

So I made an updated approach which uses the `Parse` type class and the kinds defined in `Record-Format` to effectively run the opposite of the library: parse out from a `String` value using a `Symbol` template, with parameters being used to build up a homogeneous `String` record type. I then also implemented a `ConvertRecord` type class to then allow for parsing the fields here into desired other types using type annotations.

## ParseURL

So parsing a URL will take a `Symbol` template and the `String` to be parsed, resulting in either an error (low-effort represented here as `String`) and the parsed record result:

```hs
class ParseURL (url :: Symbol) (row :: # Type) where
  parseURL :: SProxy url -> String -> Either String { | row }
```

This class has one instance, where the `Symbol` template will be converted to the `FList` defined in `Record-Format`, and this will be used to drive parsing of the string input to produce a `Record Builder` to build the record of matched parameters:

```hs
instance parseStringInst ::
  ( Parse url xs
  , ParseURLImpl xs () row
  ) => ParseURL url row where
  parseURL _ s
      = Builder.build <@> {}
    <$> parseURLImpl (FProxy :: FProxy xs) s
```

### ParseURLImpl

From our intended usage, our `ParseURLImpl` type class can be defined like so:

```hs
class ParseURLImpl (xs :: FList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  parseURLImpl
    :: FProxy xs
    -> String
    -> Either String (Builder { | from } { | to })
```

And for starters, our `FNil` instance can return a `Builder` of an empty record:

```hs
instance nilParseURLImpl :: ParseURLImpl FNil () () where
  parseURLImpl _ _ = pure identity
```

#### FCons Lit

The simpler `FCons` case to handle is the case of the `Lit`, the literal segment that we need to match for. For example, given some template `"/hello/{name}"`, the `FCons` cell for the literal will be `FCons (Lit "/hello/") tail`, which we can use to strip the prefix from the input string and apply the remainder to the tail.

```hs
instance consLitParseURLImpl ::
  ( IsSymbol segment
  , ParseURLImpl tail from to
  ) => ParseURLImpl (FCons (Lit segment) tail) from to where
  parseURLImpl _ s =
    case stripPrefix (Pattern segment) s of
      Nothing ->
        Left $ "could not strip segment " <> segment <> " from path " <> s
      Just remaining ->
        parseURLImpl (FProxy :: FProxy tail) remaining
    where
      segment = reflectSymbol (SProxy :: SProxy segment)
```

So in our example, a `String` value `"/hello/Bill"` would be stripped of `"/hello/"` and the following call to `parseURLImpl` would use the remaining string `"Bill"`.

#### FCons var

For the variable matching case, we will make a `Builder` that will insert the parsed string value into a the label inside the `Var`. Since we are working with URL strings, we delimit the string with `"/"` to split the string, or use the whole remaining string.

```hs
instance consVarParseURLImpl ::
  ( IsSymbol name
  , Row.Cons name String from' to
  , Row.Lacks name from'
  , ParseURLImpl tail from from'
  ) => ParseURLImpl (FCons (Var name) tail) from to where
  parseURLImpl _ s = do
    split' <- split
    let first = Builder.insert nameP split'.before
    rest <- parseURLImpl (FProxy :: FProxy tail) split'.after
    pure $ first <<< rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP
      split = maybe (Left "error") Right $ case indexOf (Pattern "/") s of
        Just idx -> splitAt idx s
        Nothing -> pure { before: s, after: "" }
```

And this is about it, where we compose our current `Builder` from parsing the parameter with the `Builder` produced by parsing the rest of the string.

## ConvertRecord

For the case when we want to have a heterogeneous record where some fields may be e.g. `Int`, we can choose to parse the record result further. The only problem here is that then you need to provide a type annotation for what you want the result to be typed, as the type of the fields can't be determined without that information. While I won't dig into the implementation (as it's mostly boring), you can check it out in the implementation if you'd like.

```hs
class ReadParam a where
  readParam :: String -> Either String a

class ConvertRecord (i :: # Type) (o :: # Type) where
  convertRecord :: { | i } -> Either String { | o}
```

## Usage

First, we can see how the types are fully determined. We can define a binding with a type wildcard and have the compiler give us the type:

```hs
  let (parseURL'
        :: _ )
        = parseURL (SProxy :: SProxy "/hello/{name}/{age}")
```

This will then expand:

```hs
  let (parseURL'
        -- inferred type:
        :: String -> Either String { name :: String, age :: String })
        = parseURL (SProxy :: SProxy "/hello/{name}/{age}")
```

So we can see this in action as we throw a test case at it:

```hs
main :: Effect Unit
main = do
  let (parseURL'
        -- inferred type:
        :: String -> Either String { name :: String, age :: String })
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

And as we expected, the type of the `r` in the right branch is known to have fields `name` and `age` which are both `String`.

And in the case that we want to convert the record fields, all we need to do is provide a type annotation for `r`:

```hs
  let parseURL2' = parseURL (SProxy :: SProxy "/hello/{name}/{age}")
  let parsed2 = parseURL2' "/hello/Bill/12"
  case convertRecord =<< parsed of
    Left e -> do
      log $ "didn't work: " <> e
      assert $ 1 == 2
    Right (r :: { name :: String, age :: Int }) -> do
      assert $ r.name == "Bill"
      assert $ r.age == 12
```
Amazing.

## Conclusion

Hopefully this has shown you that with 0.12 giving us the ability to effectively parse `Symbol`, we can create easy-to-use libraries that give us a lot of power with very little work. I think there are plenty of things people could do with template `Symbols` that we haven't found out yet that could make for even nicer solutions.

## Links

* This library <https://github.com/justinwoo/purescript-kushiyaki>
* Well-typed printfs cannot go wrong <http://kcsongor.github.io/purescript-safe-printf/>
* PureScript 0.12-rc1 release notes <https://github.com/purescript/purescript/releases/tag/v0.12.0-rc1>
