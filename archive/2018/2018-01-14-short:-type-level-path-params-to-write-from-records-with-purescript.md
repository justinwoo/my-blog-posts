# Short: Type-level Path Params to write from Records with PureScript

Continuing from [last time](https://qiita.com/kimagure/items/4f5c6054870f631ff768) when I wrote about parsing paths from strings using type-level path information to records, I'll now show how easy it is to write paths from records from the model we had last time.

## Review

So the model we had last time looks like this:

```hs
type MyRoute = S "hello" / S "world" / Param "id" Int / Param "name" String
```

Where `S` is an alias for Symbol Proxy, `/` is an operator alias for `Tuple`, and `Param` is defined `data Param (label :: Symbol) ty = Param`. We then had typeclass mechanisms to derive a parsing operation for a given string using this information, such that:

```hs
myRouteP :: Proxy MyRoute
myRouteP = Proxy

testUrl :: String
testUrl = "/hello/world/1/joe"

parsed :: Either BadTimes { id :: Int, name :: String }
parsed = parseUrl myRouteP testUrl
```

Which, in this case, would give us `Right { id: 1, name: "joe" }`. Now let's apply the reverse.

## Write Params

As the complement of ParseParam we can write WriteParam, but in this case we don't have to worry about failure operations since you will always be able to write to the path string from a given instance.

```hs
-- | Typeclass for writing URL segments
class WriteParam a where
  writeParam :: a -> String

instance stringWriteParam :: WriteParam String where
  writeParam s = s

instance intWriteParam :: WriteParam Int where
  writeParam i = toStringAs decimal i
```

## Writing the URL using our type and Record of params

For writing the URL, we already have the whole row type of the record fields available to us. Like with WriteParam, we will always be able to write out our URL:

```hs
class WriteURLImpl xs (row :: # Type)
  | xs -> row where
  writeURLImpl ::
       Proxy xs
    -> Record row
    -> String
```

Then our instances end up being not very much work, so for the case of tuples, we will just write the left, the right, and append them together.

```hs
instance tupleWriteURL ::
  ( WriteURLImpl left row
  , WriteURLImpl right row
  ) => WriteURLImpl (left / right) row where
  writeURLImpl _ r =
    left <> right
    where
      left = writeURLImpl (Proxy :: Proxy left) r
      right = writeURLImpl (Proxy :: Proxy right) r
```

Then each segment is a matter of reflecting the symbol to a string and formatting it:

```hs
instance segmentWriteURL ::
  ( IsSymbol segment
  ) => WriteURLImpl (SProxy segment) row where
  writeURLImpl _ _ =
    "/" <> reflectSymbol (SProxy :: SProxy segment)
```

The param instance then uses `RowCons` to ensure that the row type we have provided (via our `Record row` / `{ | row }`) contains the field we need, at the symbol `label` with the `ty` type. As we only care about the existence of the field and not about the sub-row type that is the product of constructing this row type, we can use a throwaway type variable for this.

After applying `RowCons` constraints, we gain access to `Record.get` to be able to retrieve the field's value and use it to pass into WriteParam to write our path.

```hs
instance paramWriteURL ::
  ( IsSymbol label
  , RowCons label ty trash row
  , WriteParam ty
  ) => WriteURLImpl (Param label ty) row where
  writeURLImpl _ r =
    "/" <> param
    where
      x = get (SProxy :: SProxy label) r
      param = writeParam x
```

And that's all! We can just make an alias for this method to expose for usage:

```hs
writeUrl :: forall row xs
   . WriteURLImpl xs row
  => Proxy xs
  -> { | row }
  -> String
writeUrl = writeURLImpl
```

## Usage

We'll use the same example type as last time, and then instead of using an output `{ id :: Int, name :: String }`, in this case we'll be supplying it to output the path:

```hs
type MyRoute = S "hello" / S "world" / Param "id" Int / Param "name" String

myRouteP :: Proxy MyRoute
myRouteP = Proxy

-- ....
    test "writeUrl works" do
      assert "writes out /hello/world/1/joe" $
        writeUrl myRouteP { name: "joe", id: 1 } == "/hello/world/1/joe"
```

## Conclusion

Hopefully this has shown that being able to work with type-level information not only makes it easier to get guarantees about operations in your program, but also can give you flexibility like this, where the same information can be used in multiple contexts. In this case, we've used the same information for both a "server context" to parse out parameters (which could be used with express or something) and a "client context" to prepare parameterized path strings.

If our "client context" were not in PureScript and did not include such useful features, then code could be generated for it, the same being for the "server context". *Maybe if you're a madman, you will use PureScript solely for domain type modeling and nothing else, who knows.*

## Links

* This repo: https://github.com/justinwoo/purescript-la-galbi

