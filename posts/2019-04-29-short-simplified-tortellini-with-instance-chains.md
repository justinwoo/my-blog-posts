# Short: Simplified Tortellini with Instance Chains

In 2017, I wrote about writing an INI library in PureScript (and Haskell) [here](https://qiita.com/kimagure/items/941c22effff608dda9a7). Since that post, one major feature that impacts this library was released in v0.12.0: Instance Chain groups (see <https://github.com/purescript/purescript/issues/2315>).

## What was wrong with the original?

In the original article, we needed to convert parts of an INI document which was represented by this structure:

```
top level document

  [section header 1]
    field_name_a=field_value_a
    field_name_b=field_value_b

  [section header 2]
    field_name_a=field_value_a
    field_name_b=field_value_b
```

With this structure, we only really needed one generalized operation to be able to read multiple children from a parent, i.e. document -> sections, section -> fields. However, there was no clean way to apply the same application, and we had two classes to accomplish this:

```purs
class ReadDocumentSections (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  readDocumentSections ::
       RLProxy xs
    -> StrMap (StrMap String)
    -> Except UhOhSpaghettios (Builder (Record from) (Record to))

class ReadSection (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  readSection ::
       RLProxy xs
    -> StrMap String
    -> Except UhOhSpaghettios (Builder (Record from) (Record to))
```

While most everything looks to be the same, there is one critical difference we need to deal with: documents are represented by `StrMap (StrMap String)`, while sections are represented as `StrMap String`. Previously, we handled this by having two separate type classes, but this is overall unnecessary duplication. We can merge the two classes by putting the input type in the instance head:

## Updates

```purs
class ReadLevel
  (xs :: RowList)
  (from :: # Type) (to :: # Type)
  strmap
  | xs strmap -> from to
  where
    readLevel :: RLProxy xs -> strmap -> Except UhOhSpaghettios (Builder { | from } { | to })
```

Then we can first define the base case where we close up the record:

```purs
instance nilReadLevel :: ReadLevel Nil () () strmap where
  readLevel _ _ = pure identity
```

The section level can now be written as `Object String` (`Object` == `StrMap` in 0.12) in the instance head:

```purs
instance consReadLevelSection ::
  ( IsSymbol name
  , Row.Cons name ty from' to
  , Row.Lacks name from'
  , ReadIniField ty
  , ReadLevel tail from from' (Object String)
  ) => ReadLevel (Cons name ty tail) from to (Object String) where
```

Then, while we *could* write the instance for document as the concretely typed nested `Object`, we really could make life easier by writing a polymorphic instance. However, as `forall a. Object a` overlaps with `Object String`, we need to ensure that the `Object String` instance comes first and apply instance chain groups. What all this means in the end is that `else` goes on the end.

```purs
else instance consReadLevelDocument ::
  ( IsSymbol name
  , Row.Cons name (Record inner) from' to
  , Row.Lacks name from'
  , RowToList inner xs
  , ReadLevel xs () inner a
  , ReadLevel tail from from' (Object a)
  ) => ReadLevel (Cons name (Record inner) tail) from to (Object a) where
```

And that's it!

## Conclusion

Hopefully this has shown you one example of how some type class code that might previously have required duplication or overly specific instance heads can be simplified with instance chains.

## Links

* This library: <https://github.com/justinwoo/purescript-tortellini>
