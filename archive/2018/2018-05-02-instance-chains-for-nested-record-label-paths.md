# Instance Chains to get nested record label "paths"

With the release of PureScript 0.12-rc, there are a plethora of new features that have been in development for almost a year that are now available, with one of the most interesting being "instance chains". While they allow for all kinds of new solutions to problems that before required awkward workarounds, silly hacks, or were simply not possible at the type level before, I'll go over a sample problem that helps introduce the feature.

## Nested label "paths"

This is a surprisingly common dynamic language problem that come up: "what 'paths' do I have in my HashMap?" For example, given some HashMap that you "know" you will constrain to having the shape `{ apple :: { banana :: { cherry :: String } } }`, the possible paths are

* `apple`
* `apple.banana`
* `apple.banana.cherry`

where the possible values are undefined/null/string/number/boolean/hash, where your usage coerces them to be fewer but at least two of the above.

## Setup

In PureScript, since we actually use record types, we actually know what the static structure of this should be. And so, we should be able to produce a type-level list of Symbols (statically known type-level strings), and we can define the list as such:

```hs
foreign import kind SList
foreign import data SNil :: SList
foreign import data SCons :: Symbol -> SList -> SList
```

Then we can define a proxy for moving this type around, and some operations for how to work with these. First, the proxy:

```hs
data SLProxy (xs :: SList) = SLProxy
```

Then we should have a way to append these lists, so we can collect the results up:

```hs
class SListAppend (l :: SList) (r :: SList) (o :: SList)
  | l r -> o
```

where the fundeps declare that the output is determined by the left and right arguments. The method I used to implement the append was simple: shove all the items from the left list into the right, such that when the left is nil, I simply return the right, and when the left is cons, I produce an output with the head symbol and then append the remainder of the left to the right to produce the sublist output:

```hs
instance nilSLA :: SListAppend SNil o o
instance consSLA ::
  ( SListAppend tail r o'
  ) => SListAppend (SCons s tail) r (SCons s o')
```

The other operation I needed was to map an operation to prefix the keys to a list, so that in the case of the `banana` and `banana.cherry` paths I would get from above, I could prefix them with `apple.`. I did this part with a normal class taking a prefix argument and the input, iterating through the input list to produce the output:

```hs
class SListPrefixItems (prefix :: Symbol) (i :: SList) (o :: SList)
  | prefix i -> o
instance nilSLPI :: SListPrefixItems prefix SNil SNil
instance consSLPI ::
  ( SListPrefixItems prefix tail o'
  , Append prefix "." prefix'
  , Append prefix' s s'
  ) => SListPrefixItems prefix (SCons s tail) (SCons s' o')
```

With that setup, we're done with setting up our `SList` kind and its operations.

## NestedLabels

So then let's define our class that will take in any type and produce an `SList` of the paths.

```hs
class NestedLabels a (xs :: SList) | a -> xs
```

So this "should" be "easy", where we write an instance once for `a` being `Record row` and once more for `a` being any other type.

## Previous problems

The problem with this is that an instance matching any type `a` is more general than an instance matching specifically `Record row`, i.e. `Record row` is a specific type in the set of all types. These give you overlapping instances, which can end up having really silly resolution behaviors. In PureScript, one very hacky way to work with these before was to use the lexicographic ordering of the instance names to decide which you would want to be matched for last. So in the case of `Record row` vs any `a`,

```hs
instance zzzNL :: NestedLabels a SNil
instance recordNL ::
  ( RowToList row rl
  , NestedLabelsFields rl xs
  ) => NestedLabels { | row } xs
```

The `zzzNL` instance would be matched last such that any non-match on `Record row`/`{ | row }` would use this instance, and the lexicographically earlier `recordNL` would be tried first and matched for `{ | row }`, and we'd then use `RowToList` on this to get the `RowList` to apply onwards.

## A proper solution

In 0.12-rc, we now have a much more sane way to direct this instance matching behavior using instance chains. Instead of using instance names to resolve instances, we can instead write our instances in most to least specific order using the `else` keyword:

```hs
instance recordNL ::
  ( RowToList row rl
  , NestedLabelsFields rl xs
  ) => NestedLabels { | row } xs
else instance elseNL :: NestedLabels a SNil
```

And that's it! No more ugly hacks to do this, and our usage sites no longer have overlapping instance warnings. In addition, overlapping instances will be banned from 0.12 and throw errors, so you can rest easy.

## NestedLabelsFields

Continuing on, our `NestedLabelsFields` class is implemented kind of as expected, with the `Cons` instance now using the name to apply `NestedLabels` to the children inside our type and collecting up results:

```hs
class NestedLabelsFields (rl :: RowList) (xs :: SList)
  | rl -> xs
instance nilNLF :: NestedLabelsFields Nil SNil
instance consNLF ::
  ( IsSymbol name
  , NestedLabels ty children
  , SListPrefixItems name children children'
  , NestedLabelsFields tail rest
  , SListAppend (SCons name children') rest result
  ) => NestedLabelsFields (Cons name ty tail) result
```

With a helper method, we're ready to extract these paths:

```hs
nestedLabels
  :: forall a xs
   . NestedLabels a xs
  => Proxy a
  -> SLProxy xs
nestedLabels _ = SLProxy
```

## Usage

First, let's define a nested record type that goes quite a few levels deep:

```hs
type MyRecord =
  { apple ::
  { banana ::
  { cherry ::
  { dairy ::
  { eagle ::
  { thing :: String }}}}}}
```

We can then apply this to our `nestedLabels` method with a wildcard and have the compiler solve this for us:

```hs
labels :: _
labels = nestedLabels (Proxy :: Proxy MyRecord)
```

And using our IDE tools, this wildcard then expands:

```hs
labels
  :: SLProxy
      (SCons "apple"
      (SCons "apple.banana"
      (SCons "apple.banana.cherry"
      (SCons "apple.banana.cherry.dairy"
      (SCons "apple.banana.cherry.dairy.eagle"
      (SCons "apple.banana.cherry.dairy.eagle.thing" SNil))))))
labels = nestedLabels (Proxy :: Proxy MyRecord)
```

Amazing, with very little code for the actual labels extraction implementation we've gotten all of the paths in our record.

To then print this out, we'll need a little helper to print out our `SList`:

```hs
class SListToStrings (xs :: SList) where
  sListToStrings :: SLProxy xs -> List String
instance nilSLTS :: SListToStrings SNil where
  sListToStrings _ = mempty
instance consSLTS ::
  ( IsSymbol s
  , SListToStrings tail
  ) => SListToStrings (SCons s tail) where
  sListToStrings _ =
    let
      first = reflectSymbol (SProxy :: SProxy s)
      rest = sListToStrings (SLProxy :: SLProxy tail)
    in
      List.Cons first rest
```

Then we can print away even without the separate binding:

```hs
main :: Effect Unit -- look ma, no effect rows!
main = do
  traverse_ log (sListToStrings labels)
  traverse_ log (sListToStrings $
    nestedLabels (Proxy :: Proxy MyRecord))

-- apple
-- apple.banana
-- apple.banana.cherry
-- apple.banana.cherry.dairy
-- apple.banana.cherry.dairy.eagle
-- apple.banana.cherry.dairy.eagle.thing
```

## Conclusion

Hopefully this has shown you a small demonstration of how one overlapping instance problem was solved by using instance chains. If you'd like to see an example of instance chains being used in conjunction with other additions to 0.12, be sure to check out some examples like the [safe-printf library](https://github.com/kcsongor/purescript-safe-printf).

## Links

* This repo <https://github.com/justinwoo/get-nested-keys>
* PureScript 0.12-rc1 release notes <https://github.com/purescript/purescript/releases/tag/v0.12.0-rc1>
