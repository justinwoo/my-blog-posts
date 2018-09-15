# Reflecting a record of proxies and keys of row types

The more I use anything related to type information, the more I've found that I don't want to actually write out the proxies that I know I'll need to use, so I usually end up putting these into records.

But the problem I have is that when I put these into records, I really only want to provide the type information, and I don't want to have to always update the value, as it's just tedious. For example:

```hs
myProxies ::
  { a :: Proxy Int
  , b :: Proxy String
  , c :: Proxy Unit
  }
myProxies =
  { a: Proxy
  , b: Proxy
  , c: Proxy
  }
```

Since I already have this type information, surely I can just reflect this record with my proxies? And I can!

## Reflecting the record proxy

We can make a class that will reflect the value directly:

```hs
class ReflectRecordProxy a where
  reflectRecordProxy :: a
```

Then as usual, we can make our definition such that for a record, we take the row type and make a builder for it, where the builder will be supplied by another class which will make the builder out of the RowList of the row type:

```hs
instance reflectRecordProxyInst ::
  ( RL.RowToList r rl
  , ReflectRecordProxyBuilder rl () r
  ) => ReflectRecordProxy { | r } where
  reflectRecordProxy = Builder.build builder {}
    where
      builder = reflectRecordProxyBuilder (RLProxy :: RLProxy rl)
```

*An explanation of Record.Builder can be found in some of my older posts like here: <https://github.com/justinwoo/my-blog-posts#short-composed-modified-json-parsing-for-free-with-simple-json>*

Then for the individual builders, we know that we will want to reflect any proxy type, so we can define another class that allows for reflecting any proxy:

```hs
class ReflectRecordProxyBuilder (rl :: RL.RowList) (i :: # Type) (o :: # Type)
  | rl -> i o where
  reflectRecordProxyBuilder :: RLProxy rl -> Builder { | i } { | o }

instance reflectRecordProxyBuilderNil :: ReflectRecordProxyBuilder RL.Nil () () where
  reflectRecordProxyBuilder _ = identity

instance reflectRecordProxyBuilderConsRoute ::
  ( ReflectRecordProxyBuilder tail from from'
  , Row.Lacks name from'
  , Row.Cons name a from' to
  , ReflectProxy a
  , IsSymbol name
  ) => ReflectRecordProxyBuilder (RL.Cons name a tail) from to where
  reflectRecordProxyBuilder _ = first <<< rest
    where
      first = Builder.insert (SProxy :: SProxy name) reflectProxy
      rest = reflectRecordProxyBuilder (RLProxy :: RLProxy tail)

-- | Various proxies that can be created
class ReflectProxy a where
  reflectProxy :: a

instance reflectProxyProxy :: ReflectProxy (Proxy a) where
  reflectProxy = Proxy

instance reflectProxySProxy :: ReflectProxy (SProxy s) where
  reflectProxy = SProxy

instance reflectProxyRProxy :: ReflectProxy (RProxy s) where
  reflectProxy = RProxy

instance reflectProxyRLProxy :: ReflectProxy (RLProxy s) where
  reflectProxy = RLProxy
```

And this is actually all we need.

## Putting this to use

Now we can simply supply only the type annotation and reflect the record value for free:

```hs
proxies = N.reflectRecordProxy ::
  { apple :: Proxy Int
  , banana :: Proxy String
  }
```

And we can use this with our own proxy data type by making an instance of `ReflectProxy`:

```hs
data MyThing a b c d e f g = MyThing

instance myThingReflectProxy :: N.ReflectProxy (MyThing a b c d e f g) where
  reflectProxy = MyThing

things = N.reflectRecordProxy ::
  { apple :: MyThing Int Int Int Int Int Int Int
  , banana :: MyThing Unit Unit Unit Unit Unit Unit Unit
  }
```

## Bonus: collecting row keys

Say we have a concrete record type where we want to collect the keys and make an SProxy record. This ends up being not too much work either, where we first RowToList on the row type we are working off of and build up a row type by using Row.Cons constraints on it:

```hs
class GetKeysRow (r :: # Type) (keys :: # Type) | r -> keys

instance getKeysRowInst ::
  ( RL.RowToList r rl
  , GetKeysRowInst rl keys
  ) => GetKeysRow r keys

class GetKeysRowInst (rl :: RL.RowList) (keys :: # Type)
  | rl -> keys

instance getKeysRowInstNil :: GetKeysRowInst RL.Nil ()

instance getKeysRowInstCons ::
  ( GetKeysRowInst tail keys'
  , Row.Cons name (SProxy name) keys' keys
  ) => GetKeysRowInst (RL.Cons name ty tail) keys
```

Then we can define a variety of functions that will extract out the keys of a row type. For our purposes, we're mostly interested in proxies of types that act as row proxies, meaning some kind of data type of the kind signature

```
SomeDataType :: # Type -> Type
```

For which we know of a few:

```hs
-- from Prim
foreign import data Record :: # Type -> Type

-- from typelevel-prelude
data RProxy (row :: # Type) = RProxy

-- from variant
foreign import data Variant :: # Type -> Type
```

So we'll just write a function that takes any data type of that kind:

```hs
getKeysRecord'
  :: forall proxy rproxy r keys
   . GetKeysRow r keys
  => N.ReflectRecordProxy { | keys }
  => proxy (rproxy r)
  -> { | keys }
getKeysRecord' _ = N.reflectRecordProxy
```

*I have previously written about polymorphic proxies here: <https://github.com/justinwoo/my-blog-posts#polymorphic-proxy-fun-in-purescript>*

## Putting it all together

First, we can just write out the application of this function with a Proxy of a record type:

```hs
keys = X.getKeysRecord' (Proxy :: Proxy { a :: Int, b :: Int, c :: Int })
```

And the PureScript IDE Server will give us this message:

```
No type declaration was provided for the top-level declaration of keys.
  It is good practice to provide type declarations as a form of documentation.
  The inferred type of keys was:

    { a :: SProxy "a"
    , b :: SProxy "b"
    , c :: SProxy "c"
    }


in value declaration keys
```

And we can use the inferred signature by applying the IDE suggestion in our editor:

```hs
keys ::
  { a :: SProxy "a"
  , b :: SProxy "b"
  , c :: SProxy "c"
  }
keys = X.getKeysRecord' (Proxy :: Proxy { a :: Int, b :: Int, c :: Int })
```

And then we can use any of the properties of `keys` to work with things like `Record.insert` and other functions that take SProxy values.

## Conclusion

I hope this has shown that we can really derive a lot of values if we have enough type information, and that runtime values can fill in the gaps where we have them.

I also really hope that my links to my previous posts show that all of these blog posts just small parts of a large continuous whole, so nothing here is too different from the material covered in previous posts.

## Links

* Naporitan, for reflect record proxy: https://github.com/justinwoo/purescript-naporitan
* Xiaomian, for getting keys records: https://github.com/justinwoo/purescript-xiaomian

#### P.S.

Is anyone interested in helping me write a Japanese post for Qiita? I have been thinking of writing a simpler version of [Type classes and instances are pattern matching for types](https://qiita.com/kimagure/items/08c59fa21adcd6968ae1) that would be useful for those who read my posts mostly for the code but not much of the actual words. My Japanese isn't very good, but hopefully you can help me fix many details if we start working on this.
