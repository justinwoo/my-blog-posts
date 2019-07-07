# Mapping a function to a homogeneous record in PureScript

If you're reading this blog post, chances are that you've seen my talk ["RowList Fun with PureScript"](https://www.reddit.com/r/purescript/comments/6xs5f2/rowlist_fun_with_purescript_slides_from_small_fp/), whether that was online, YouTube, ClojuTRE/Small FP Conf, HaskellX Hackathon, etc. While you might want to read the slides from that talk if you haven't seen the talk, it won't really be necessary for what I'll go over here.

I'll try to explain all the important details needed for a relatively new PureScript user to do this themselves. When in doubt, see [PureScript by Example](https://leanpub.com/purescript/read), ask on [#purescript in the FP Slack](https://fpchat-invite.herokuapp.com/), and/or complain on Twitter #purescript

## Some Background

Every now and again, people want to map a function over the fields of a record where all the fields are the same type. This is more common in dynamic languages, but sometimes people want to do this for statically typed languages also. There are multiple reasons why you should **not** just give up and use a StringMap here:

* You know which keys are statically available.
* If you use a StringMap, you lose the ability to work with information from the keys statically.
* You can carry evidence that certain fields will always be defined to coerce lookups, but then you will be implementing something that is not a record even then.
* JavaScript libraries you work with may need typing for these kinds of operations, so a StringMap would be technically wrong to use.

## What

A row type is an unordered collection of fields, where there is an associated label that is a type-level string and a field that is normally a `Type` (though you may find many uses for non-Type fields). This really just means that a type level "label" is available for the record definition `{ label :: String}`, so a binding `record` with this type is able to use `record.label`. The row of fields is defined as `# Type` in the kind signature, the "type of the type" of the row type.

In PureScript, a Record takes a row type parameter to yield a type, so we can easily work with this row type information by matching on it at the type level. The above definition `{ label :: String }` is sugar for `Record ( label :: String )`, which we can then exploit for operations we need.

Since PureScript has typeclasses, we can actually match on types for operations we need. But of course, there is a problem that we can just match on some unordered blob since there is no clear way to iterate on the fields or reduce the number of fields we have left to work on. To this purpose, we have another typeclass `RowToList` that can convert these row types into a type-level `RowList`, consisting of a `Cons` element with the head element's label, type field, and the rest of the `RowList` and a `Nil` element for the empty list.

## Defining our Typeclass

For our typeclass, we're going to want the following things:

1. A `RowList` that we use to iterate fields of the record
2. A row `# Type` for the original record
3. The input type we want in our mapping function, `a`
4. The output type we want in our mapping function, `b`
5. The output row for the output record, where the fields are of `b` from the result of `a -> b`.

There are two things we need to do though for our typeclass:

1. Since we can't match on row types in instance heads in PureScript, we need to put these behind a functional dependency
2. `a` and `b` are not to be used for resolving instances, since we only want to match instances using our `RowList`

And while functional dependencies themselves can be fun to read about, in our case we only need the simplest thing, and so our typeclass definition looks like this:

```hs
class MapRecord (xs :: RowList) (row :: # Type) a b (row' :: # Type)
  | xs -> row row' a b where
  mapRecordImpl :: RLProxy xs -> (a -> b) -> Record row -> Record row'
```

So this looks like a normal map function, but with the addition of the `RowList` proxy that will be used to match the instances.

### A Review of Proxies

You might be familiar with `data Either a b = Left a | Right b` or `data Maybe a = Nothing | Just a` and such data types where some of constructors don't carry all the parameters. When you use `Nothing`, the type of `a` isn't known unless it's provided by context, or if you explicitly provide it as `Nothing :: Maybe Int`. What if we took this one step further? A `Proxy` works in this way by giving you `data Proxy a = Proxy`, where you can pass this Proxy value around and use the type parameter in various ways. In the case of `RLProxy`, this proxy defined with `a` being of kind `RowList`.

## Defining our instances

Let's start with the base case:

```hs
instance mapRecordNil :: MapRecord Nil row a b () where
  mapRecordImpl _ _ _ = {}
```

Our instance here is defined such that for a `Nil`, we also match that the output row type will be an empty row and return an empty record. The empty row is allowed to be used here as it is not a resolving member of this instance and only used for constraints.

For our cons case we will have many more things going on, but let's start with a partial definition:

```hs
instance mapRecordCons ::
  ( MapRecord tail row a b tailRow'
  ) => MapRecord (Cons name a tail) row a b row' where
  mapRecordImpl _ f r = ?something
```

This is what we want for the `Cons` case, where the field is constrained to be the same type as `a` from the mapping function, and `tail` is used in the constraints to ensure a `MapRecord` instance for the downstream subrow to be created. We also introduce a variable `tailRow'` to represent the result subrow from the downstream instance that we will use to combine with `row'`.

Now the gory details:

1. We'll need to add a `IsSymbol name` constraint to get access to operations that require a symbol
2. We need to get the property out of the record using the symbol. Doing so will require a constraint `RowCons name a trash row`, to satisfy that the row `row` contains a field `a` with the label `name` when adding this field to a supposed `trash` row.
3. We need to then insert our field with the label `name` with type `b` after applying `a -> b` to the property. This requires that the subrow that we are inserting to does not already have a field with this name, so we use `RowLacks name tailRow'` from the inner instance.
4. Then we add `RowCons name b tailRow' row'` to complete `row'`.

With this, we can use functions defined in purescript-record to get to work, such that our full definition looks like this:

```hs
instance mapRecordCons ::
  ( MapRecord tail row a b tailRow'
  , IsSymbol name
  , RowCons name a trash row
  , RowLacks name tailRow'
  , RowCons name b tailRow' row'
  ) => MapRecord (Cons name a tail) row a b row' where
  mapRecordImpl _ fn record =
    insert nameP value rest
    where
      nameP = SProxy :: SProxy name
      value = fn $ get nameP record
      rest = mapRecordImpl (RLProxy :: RLProxy tail) fn record
```

## Exposing this for usage

While you could manually prepare the `RowList` proxy and all yourself, I don't really want to have to call a function that way, so I make a wrapper like this:

```hs
mapRecord :: forall row xs a b row'
   . RowToList row xs
  => MapRecord xs row a b row'
  => (a -> b)
  -> Record row
  -> Record row'
mapRecord = mapRecordImpl (RLProxy :: RLProxy xs)
```

And then this becomes much nicer to actually use:

```hs
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print $ mapRecord ((+) 1) {a: 1, b: 2, c: 3}
  -- {"c":4,"b":3,"a":2}
  print $ mapRecord (append "shown: " <<< show) {a: 1, b: 2, c: 3}
  -- {"c":"3","b":"2","a":"1"}
```

## Conclusion

So hopefully this has shown you that working with row types is fun and gives you first-class access to record structures (and much more), rather than having to use datatype generics or StringMaps. There's a lot of information here, so please ask questions about any part of this on [#purescript in the FP Slack](https://fpchat-invite.herokuapp.com/)!

## Links

* Repo: https://github.com/justinwoo/purescript-map-record

## Post-script

For Haskell, a lot of these same things can be done quite easily if you use generics-sop with records-sop, like so: https://github.com/justinwoo/godawful-purescript-codegen-demo/blob/08f615037ec1261a9a382e739282a14face844fc/app/GeneratePS.hs#L35. Otherwise, normal GHC.Generics might be quite useful too. For the very curious, [coxswain](https://github.com/nfrisby/coxswain) might provide utilities you might want to use, but I haven't tried it.

For Scala, as far as I can tell, you can accomplish a lot of these same things using [shapeless](https://github.com/milessabin/shapeless)

Almost no other commonly used languages have a concept of datatype generics or type-level programming, so you may end up having to write this manually (for each pair of input and output types!), using a StringMap, or generating code for these operations, none of which are quite ideal.