Type-safe Record operations with Purescript-record
Previously, I talked about how we can use `RowToList` in Purescript to work with row types of records by converting them to type-level lists [here](http://qiita.com/kimagure/items/d8a0681ae05b605c5abe). But the actual implementation was a little gory and maybe brittle -- it relies on implementation details of `StrMap` and loses type safety in that it ignores the requirement of unique keys and the type of the individual fields. While we can write tests against our implementation, unsafe coercion of data doesn't really give us the most confidence.

Thankfully, [Purescript-Record](http://pursuit.purescript.org/packages/purescript-record) gives us all the type-safe operations on records with proper typings that we want. I recently went through and fixed the implementation of record building in Simple-JSON to take advantage of this library.

## Some Background

The Simple-JSON library has one main typeclass that it provides implementations to the core data structures for with this signature:

```hs
class ReadForeign a where
  readImpl :: Foreign -> F a
```

Where `F a` is an alias for `Except MultipleErrors a`. The typical instance then looks the following:

```hs
instance readString :: ReadForeign String where
  readImpl = readString
```

Where `readString` is from the Foreign library.

## The Wrong Way

The previous method would then define an instance for Record in this way:

```hs
instance readRecord ::
  ( RowToList fields fieldList
  , ReadForeignFields fieldList
  , ListToRow fieldList fields
  ) => ReadForeign (Record fields) where
  -- ...
```

By using `RowToList` and `ListToRow`, we can extract and constrain our type-level list of our record fields row type. Then we would use a `ReadForeignFields` class defined as such:

```hs
class ReadForeignFields (xs :: RowList) where
  getFields ::
    RLProxy xs
    -> Foreign
    -> F (StrMap Foreign)
```

Where `RLProxy` (RowList Proxy) is a Proxy data type for our type-level list extracted from our row type. But here, we have the return type as `StrMap Foreign`, meaning that we are parsing to a `StrMap`, which is only a `Record` by an implementation detail, of `Foreign`, or a kind of `any` JS value. The implementation of `readImpl` for the above instance then looks like this:

```hs
readImpl = unsafeCoerce $ getFields (RLProxy :: RLProxy fieldList)
```

Which is not the greatest. The implementations also involve creating an empty `StrMap` and making unions with singleton `StrMap`. Technically correct, but doesn't have many of the characteristics that we actually want. Good news is that we can solve this with Purescript-Record operations *and* get all the characteristics we want.

## Purescript-Record in a nutshell

While we could work with specific record types and add specific fields, we need a solution that works for *all* records where we can add any label of any type. Purescript-Record gives us exactly that, of which we really only need one method, [insert](https://pursuit.purescript.org/packages/purescript-record/0.1.0/docs/Data.Record#v:insert):

```hs
insert :: forall r1 r2 l a
  .  IsSymbol l
  => RowLacks l r1
  => RowCons l a r1 r2
  => SProxy l
  -> a
  -> { | r1 }
  -> { | r2 }
```

This is fairly involved, but the individual parts all come with their own documentation.

* [`IsSymbol l`](https://pursuit.purescript.org/packages/purescript-symbols/3.0.0/docs/Data.Symbol#t:IsSymbol) just adds the constraint that `l` should be a `Symbol` label
* [`RowLacks l r1`](https://pursuit.purescript.org/packages/purescript-typelevel-prelude/2.3.1/docs/Type.Row#t:RowLacks) guarantees that the row type `r1` doesn't already have a label `l`.
* [`RowCons l a r1 r2`](https://pursuit.purescript.org/builtins/docs/Prim#t:RowCons) is a feature available from the compiler that will take a label, a type, and a row to then add the label :: type field into the row to produce the result row.

That's about it. It then takes a `SProxy` ("string" Proxy) of our label, a value of our type, and a record that has the constraints to add this field into the result record.

## Putting this to work

There is one more minor detail to talk about: the relationship between a given row type and row-list can't be solved, so we do need to change our `ReadForeignFields` class accordingly:

```hs
class ReadForeignFields (xs :: RowList) (row :: # Type) where
  getFields ::
    RLProxy xs
    -> RProxy row
    -> Foreign
    -> F (Record row)
```

So the difference from above is that we don't need to extract the row type from the row-list here, as we pass in the row type through a `RProxy` (row Proxy). Our instance for the row-list elements ends up being quite straightforward:

```hs
instance readFieldsCons ::
  ( IsSymbol name
  , ReadForeign ty
  , ReadForeignFields tail tailRow
  , RowLacks name tailRow
  , RowCons name ty tailRow row
  ) => ReadForeignFields (Cons name ty tail) row where
  getFields _ _ obj = do
    value <- readImpl =<< readProp name obj
    rest <- getFields tailP tailRowP obj
    pure $ insert nameP value rest
    where
      nameP = SProxy :: SProxy name
      tailP = RLProxy :: RLProxy tail
      tailRowP = RProxy :: RProxy tailRow
      name = reflectSymbol nameP
```

Our row-list `Cons` consists of the label/name and type of the field and the remaining elements of the list. In using the `insert` method from above, we add constraints for `IsSymbol name` along with `RowLacks name tailRow` and `RowCons name ty tailRow row` appropriately, where `tailRow` is introduced by our `ReadForeignFields tail tailRow` constraint, thus, extracting out the row type from our tail row-list. The `ReadForeign ty` constraint simply makes sure that we are able to read out the value from the type.

For our `Nil` instance, however, we need to add a helper instance:

```hs
instance readFieldsNil ::
  ( TypeEquals {} (Record row)
  ) => ReadForeignFields Nil row where
  getFields _ _ _ =
    pure $ to {}
```

Because a normal `{}` by itself doesn't necessarily mean its type is equal to `Record row`, we need to supply a constraint `TypeEquals {} (Record row)`. To then do the conversion, we use the [`to`](https://pursuit.purescript.org/packages/purescript-type-equality/2.1.0/docs/Type.Equality#v:to) method to convert our `{}` type into a `Record row`.

And that's about it!

## おあがり〜

Hopefully this has shown that these type-safe record operations can be quite nice and useful in various ways, and given you some ideas on various record operations you'd also like to perform, both concrete and abstract.

## Future work

There has been talk about making a Record.ST module or something to make ST-constrained locally mutating operations readily available for cases like these where you'd like to just modify the records directly. To be continued...

## Links

* This repo: https://github.com/justinwoo/purescript-simple-json
* Purescript-Record: https://github.com/purescript/purescript-record
* More about `RowToList`: https://www.reddit.com/r/purescript/comments/6mss5o/new_in_purescript_0116_rowtolist/
