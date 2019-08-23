Many times when we work with JS libraries from PureScript, we find that many methods take in a record for their arguments that may have any fields that are a subset of a total set of valid properties. But how should we go about modeling this?

Luckily in PureScript 0.11.x, we gained the ability to talk about Unions of rows, where we can declare that the combination of rows `left` and `right` can form a total set `union`:

```hs
class Union (left :: # Type) (right :: # Type) (union :: # Type)
  | left right -> union
  , right union -> left
  , union left -> right
```

We can see from the fundeps that if we have the `left` and `union`, we can determine `right` here. How would we exploit this to work with JS APIs?

## "Unions for Partial Properties in PureScript"

In this post, I talked about how to take advantage of Union to partially type echarts:

<https://qiita.com/kimagure/items/581c63707673db61e061>

The approach used here matches that of React-Basic, a library from the folks at Lumi, where we can use the concrete input provided by the user and the total set of properties we know exist for a given method together to determine the unspecified complement. While we don't care about the complement very much, we at least can tell from this that we have specified a valid subset of inputs in this way.

This gives us the ability to talk about most JS interfaces that use partial sets of inputs, so this is definitely worth looking into the next time you run into object inputs.
