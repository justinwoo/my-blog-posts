# Formatting type-level Strings with row type labels

みなさん、ご無沙汰しております〜 It's been a while since I wrote anything here, so this time I'll write about some more `Symbol` formatting using the labels of a row type.

## What

Since PureScript 0.12, we're now able to do a lot of things with `Symbol`s, where we might use them to parse out parameters to use with SQL queries like I've demonstrated [in this post](https://qiita.com/kimagure/items/4b08e9f0479d5866ec04). While this post simply writes out the entire query, we can see some potential here where we might even construct `Symbol`s by taking the labels out of a record type that we want to parse the results to when forming a select query. And so, in this post we'll look at how we write some code to do so.

## Formatting `Symbol`s with `Symbol`s

First, we'll define a function so we can format `Symbol`s. We use the [record-format](https://github.com/kcsongor/purescript-record-format) library that we talked about previously [here](https://qiita.com/kimagure/items/3273d20c4c5ad74dbe26#record-format).

```hs
formatSymbol
  :: forall string flist row out proxyOrRec
   . RF.Parse string flist
  => FormatSymbolParsed flist row "" out
  => SProxy string
  -> proxyOrRec row
  -> SProxy out
formatSymbol _ _ = SProxy
```

Like last time, using the `Parse` class from record-format gives us a type-level list of tokens, which might be a parameter variable `Var` or a literal string `Lit`. With this information, we can define our class `FormatSymbolParsed` which will accumulate a formatted symbol:

```hs
class FormatSymbolParsed
  (flist :: RF.FList)
  (row :: # Type)
  (acc :: Symbol)
  (out :: Symbol)
  | flist -> row acc out
```

For this class, we have our functional dependencies set to say that the parameters can all be determined by the `flist`, as we only need to match instances based on it. For the empty case, we simply say that the accumulate and output are the same:

```hs
instance nilFormatSymbolParsed :: FormatSymbolParsed RF.FNil row out out
```

For the variable case, we can take the correct `Symbol` that is carried by an `SProxy` in our row type for the variable name using `Row.Cons`:

```hs
instance consVarFormatSymbolParsed ::
  ( Symbol.Append acc sym acc'
  , Row.Cons var (SProxy sym) row' row
  , FormatSymbolParsed tail row acc' out
  ) => FormatSymbolParsed (RF.FCons (RF.Var var) tail) row acc out
```

So we take the `Symbol` parameter carried in the `row` parameter and use that to append to the accumulate. In the case of the literal, we can just append the symbol as-is:

```hs
instance consLitFormatSymbolParsed ::
  ( Symbol.Append acc lit acc'
  , FormatSymbolParsed tail row acc' out
  ) => FormatSymbolParsed (RF.FCons (RF.Lit lit) tail) row acc out
```

## Intercalating row labels

To prepare the `Symbol`s that we will format with, we need to intercalate labels of a row such that given `(a :: _, b :: _, c :: _)`, we should be able to intercalate this with `, ` to get `a, b, c`. First, let's prepare the top-level function:

```hs
intercalateRowLabels
  :: forall row x out proxyOrRecord
   . IntercalateRowLabels row x out
  => proxyOrRecord row
  -> SProxy x
  -> SProxy out
intercalateRowLabels _ _ = SProxy

intercalateRecordLabels
  :: forall row x out
   . IntercalateRowLabels row x out
  => Proxy { | row }
  -> SProxy x
  -> SProxy out
intercalateRecordLabels _ _ = SProxy
```

Here I've defined two functions that do the same thing, but where one can take a `RProxy`, `Record`, or some other type with kind `# Type -> Type` and one that takes a `Proxy` of the record. Then we can define the `IntercalateRowLabels` class used here:

```hs
class IntercalateRowLabels (row :: # Type) (x :: Symbol) (out :: Symbol)

instance intercalateRowLabelsInstance ::
  ( RL.RowToList row rl
  , IntercalateRowLabelsImpl rl x "" out
  ) => IntercalateRowLabels row x out
```

So this is a type class with a single instance which will kick off into the implementation `IntercalateRowLabelsImpl` with an empty string for the accumulate.

```hs
class IntercalateRowLabelsImpl
  (rl :: RL.RowList)
  (x :: Symbol)
  (acc :: Symbol)
  (out :: Symbol)
  | rl -> x out
```

Like before, our accumulator function will match instances on the `RowList` parameter, and the empty case will match the accumulator to the output:

```hs
instance nilIntercalateRowLabelsImpl :: IntercalateRowLabelsImpl RL.Nil x out out
```

Then there are two cases to handle for intercalation: the last and Nth elements. For the last element, we can just add the last label directly:

```hs
instance consNilIntercalateRowLabelsImpl ::
  ( Symbol.Append acc name acc'
  ) => IntercalateRowLabelsImpl (RL.Cons name ty RL.Nil) x acc acc'
```

Then for the Nth element, we can write a chained instance that first appends the label and delimiter to use in appending to the accumulate:

```hs
else instance consIntercalateRowLabelsImpl ::
  ( Symbol.Append name x s
  , Symbol.Append acc s acc'
  , IntercalateRowLabelsImpl tail x acc' out
  ) => IntercalateRowLabelsImpl (RL.Cons name ty tail) x acc out
```

And that's all we need here.

## Usage

We can see our `intercalateRecordLabels` function at work:

```hs
type MyRecord =
  { a :: SProxy "A"
  , b :: SProxy "B"
  , c :: SProxy "C"
  }

-- inferred type:
labels :: SProxy "a, b, c"
labels =
  S.intercalateRecordLabels
    (Proxy :: Proxy MyRecord)
    (SProxy :: SProxy ", ")
```

The type signature here for `labels` is inferred, so we can change other types and have our IDE plugin generate the type signature again.

We can also see an inferred example in action:

```hs
main = do
  let
    myLabels =
      S.intercalateRecordLabels
        (Proxy :: Proxy { apple :: Int, banana :: String })
        (SProxy :: SProxy ", ")
    myFormatted =
      S.formatSymbol
        (SProxy :: SProxy "myLabels: {myLabels}")
        { myLabels }

  assertEqual
    { actual: reflectSymbol myFormatted
    , expected: "myLabels: apple, banana"
    }
```

Works like a charm!

## Conclusion

This time, we mostly just reused methods that we've seen in my earlier posts about `Symbol.Cons** and instance chains in PureScript 0.12. Hopefully this has shown you that you can also come up with many of your solutions to these problems, once you know how to work with the constraints and functional dependencies.

## Links

* Repo: https://github.com/justinwoo/purescript-shoronpo
* Previous post about using record-format to have typed path params: https://qiita.com/kimagure/items/3273d20c4c5ad74dbe26
* Previous post about parsing `Symbol`s for type annotations: https://qiita.com/kimagure/items/6729a5d55ab99bcee8ec

