If you've seen any of my codegen posts, you'll know that I use `Proxy` in a bunch of places in order to provide the types for instance resolving, so that I can extract the string name I want to use for my generated code.

## Normal Proxy Usage

So the normal class definition looks something like this:

```hs
class GetName a where
  getName :: Proxy a -> String
  
instance getNameInt :: GetName Int where
  getName _ = "Int"
```

So to get the name from the `Int` type, I would create a `Proxy` like so:

```hs
main = do
  log $ getName (Proxy :: Proxy Int)
```

But wait, what if I already had a value defined?

```hs
two :: Int
two = 2
```

To create a `Proxy` out of this, I would have to define a function like so:

```hs
mkProxy :: forall a. a -> Proxy a
mkProxy _ = Proxy
```

But this isn't so simple in some other cases. Say we had a class for extracting keys from a `RowList`, then we'd have to provide a function for providing a row type to work with this:

```hs
class Keys (xs :: RowList) where
  keysImpl :: RLProxy xs -> List String
  
keys :: forall row rl
   . RowToList row rl
  => Keys rl
  => RProxy row
  -> List String
keys _ = keysImpl (RLProxy :: RLProxy rl)

mkRProxyFromRecord :: forall row
   . Record row
  -> RProxy row
mkRProxyFromRecord _ = RProxy
```

But do we really need to bother with always working with a concretely typed `Type`-kinded Proxy and `# Type`-kinded RProxy?

## Concrete to polymorphic

Luckily, no, we don't. By taking advantage of the fact that we can create higher kinded types in PureScript, we can define a type variable that will contain the inner type. So the above `keys` function can be rewritten:

```hs
keys :: forall g row rl
   . RowToList row rl
  => Keys rl
  => g row -- this will work for any type with the row as a param!
  -> List String
keys _ = keysImpl (RLProxy :: RLProxy rl)
```

So now this function can be called with either a `RProxy`, a normal `Record`, or even a `Variant`.

We can rewrite the `GetName` class to use this instead:

```hs
class GetName a where
  getName :: forall f. f a -> String

instance getNameInt :: GetName Int where
  getName _ = "Int"
```

And now we can call `getName` with either a Proxy or anything that is `Type -> Type` kinded, i.e. contains our value as an inner type at the type level:

```hs
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ getName (Proxy :: Proxy Int)
  log $ getName (Identity two)
```

And ta-da, we don't have to make some `Proxy`-making function or anything just to work with concrete values' types.

## Conclusion

Hopefully this has shown you that you can always replace any concrete type with a polymorphic one if you aren't using all of the operations associated with them. And even in the cases where you are using some operations of them, e.g. Semigroup append of List items, you can make sure that no unwanted operations are used by having a polymorphic variable that is then constrained for the right type classes and their methods.

## Links

* This demo https://github.com/justinwoo/polymorphic-proxy-demo

## P.S.

Unfortunately this only works if your language has higher kinded types and higher ranked types. Without the former, you can't declare `f a` / `f :: Type -> Type`, and the latter `forall f.` quantification in the type class method.

Haskellers using GHC8 may opt to use Type Applications with Allowed Ambiguous Types to not use proxies altogether, though you will need a solution for extracting types from values.