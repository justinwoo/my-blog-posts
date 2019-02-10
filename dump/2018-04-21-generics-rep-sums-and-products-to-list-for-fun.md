---
title: Generics-Rep Sums and Products to List for Fun
tags: purescript
author: kimagure
slide: false
---
Sometimes when I write PureScript code, I want to do something as easy as using row types with `RowToList` when dealing with normal ADTs, but I don't quite have as convenient of ways to work with ADTs: when I deal with (Polymorphic) `Variant`s instead of sums, I can simply `RowToList` on the row type and work with the type level-list. Likewise, I when I deal with `Record` problems instead of products, I can simply `RowToList` again on the row type and work with the type-level list. But why not for normal ADT types' Generic Representations? Well, very well could!

## `Sum` to `SumList1`

One thing to keep in mind is that the `Rep` of a sum type will always be a non-empty list of at least two elements, as you can't have a left or right side otherwise. What do I mean? Consider a simple sum type:

```hs
data MySum = First | Second
derive instance genericMySum :: Generic MySum _
```

Let's then see what the rep is by using a wildcard signature and expanding it:

```hs
from :: MySum -> _ -- expands to:
                   -- Sum
                   --   (Constructor "First" NoArguments)
                   --   (Constructor "Second" NoArguments)
from = GR.from
```

We can see that the associated rep has two elements under the sum, and in the actual definition of `Sum` we can see that there are the `Inl` and `Inr` constructors for the rep:

```hs
data Sum a b = Inl a | Inr b
```

For my own uses though, I smashed this into a non-empty list of one, since I don't care to try to handle too many details at once. And so my `SumList1` definition's type-level definition is as follows:

```hs
-- | a non-empty list made of a Generic Sum's type elements
foreign import kind SumList1
-- | the base element of a Generic Sum
foreign import data Sum1 :: Symbol -> Type -> SumList1
-- | the N-th element of a Generic Sum
foreign import data SumN :: Symbol -> Type -> SumList1 -> SumList1
```

Where the `Symbol` is the name of the `Constructor` above and `Type` is the argument position type. Practically, this is the same as a Haskell GADT defined with constructor signatures (see <https://en.wikibooks.org/wiki/Haskell/GADT#Summary>):

```hs
-- this is Haskell GADTs, not PureScript
data List a where
   Nil  :: List a
   Cons :: a -> List a -> List a
```

Now, to be able to easily pass around this type-level information for the type class we're going to define and use, we need to define a Proxy:

```hs
-- | a Proxy for SumList1
data SLProxy (list :: SumList1) = SLProxy
```

### `SumToList`

Let's define our class and its instances now. We know that this class will take the sum type in and produce our `SumList1`, and especially that we want our instances to be matched based on the sum type parameter (and that the list is determined by the sum type):

```hs
class SumToList sum (list :: SumList1) | sum -> list
```

From here, one thing we can readily do is take advantage of the fact that sum type reps are always right-sided, so the left item will always be a constructor. With this knowledge, we don't need to bother with writing any append operation and can pop off items from the sum as a list readily:

```hs
instance sumSumToList ::
  ( SumToList b r
  ) => SumToList (Sum (Constructor name a) b) (SumN name a r)
```

So for the `Sum` case, we take the `name` and type `a` from the left and create a N-th list item, then applying the rest of the list produced by running `SumtoList` further on the right side `b`.

Finally, the last item of the sum will also be a constructor, so the other instance we need is to then match `Constructor` and make a `Sum1` element to finish the list:

```hs
instance conSumToList ::
  SumToList (Constructor name a) (Sum1 name a)
```

And these are the only instances we need to get going with simple sum types.


## `Product` to `ProductList1`

The product type case is largely the same, but starts with a `Constructor` that then has the following arguments as a `Product`:

```hs
data MyProduct = MyConstructor Int String
derive instance genericMyProduct :: Generic MyProduct _

from :: MyProduct -> _ -- expands to
                       -- Constructor
                       --   "MyConstructor"
                       --   (Product
                       --     (Argument Int)
                       --     (Argument String))
from = GR.from
```

In this case, since the arguments will be normal types, we won't have a name parameter:

```hs
-- | a non-empty list made of a Generic Product's type elements
foreign import kind ProductList1
-- | the base element of a Generic Product
foreign import data Product1 :: Type -> ProductList1
-- | the N-th element of a Generic Product
foreign import data ProductN :: Type -> ProductList1 -> ProductList1

-- | a Proxy for ProductList1
data PLProxy (list :: ProductList1) = PLProxy
```

### `ToProductList`


Just to unwrap the constructor, I made a convenience class with a single instance to extract out the rep to the name and the actual list:

```hs
-- | convenience class to apply ProductToList on a data type directly
class ToProductList product (name :: Symbol) (list :: ProductList1) | product -> name list

instance toProductList ::
  ( ProductToList product list
  ) => ToProductList (Constructor name product) name list
```

### `ProductToList`

Like before, the `Product` rep is right-sided, so we can take items from the head and create the N-th list item with this, and then match `Argument` to make the last list item:

```hs
class ProductToList product (list :: ProductList1) | product -> list

instance productProductToList ::
  ( ProductToList b r
  ) => ProductToList (Product a b) (ProductN a r)

instance argProductToList ::
  ProductToList (Argument a) (Product1 (Argument a))
```

## Usage

### `SumList1` usage

Like with any class using `RowList`, we write a class to the list and handle the base and increment cases. The main diference with typical `RowToList` classes is that since we have a non-empty list, our base repeats the increment case actions, and we can return the results as a `NonEmptyList` for a class such as `SumNames`:

```hs
class SumNames (list :: SumList1) where
  sumNames :: SLProxy list -> NonEmptyList String

instance zSumNames ::
  ( IsSymbol sumName
  ) => SumNames (Sum1 sumName ty) where
  sumNames _ = pure $ reflectSymbol (SProxy :: SProxy sumName)

instance sSumNames ::
  ( IsSymbol sumName
  , SumNames tail
  ) => SumNames (SumN sumName ty tail) where
  sumNames _ = head <> rest
    where
      head = pure $ reflectSymbol (SProxy :: SProxy sumName)
      rest = sumNames (SLProxy :: SLProxy tail)
```

We can then apply this to a sum type that derives a `Generic`s-Rep instance accordingly:

```hs
data Fruit
  = Apple
  | Banana
  | Cherry
derive instance genericFruit :: Generic Fruit _

availableFruits :: NonEmptyList String
availableFruits = availableFruits'
  where
    availableFruits'
      :: forall rep list
      . Generic Fruit rep
      => SumToList rep list
      => SumNames list
      => NonEmptyList String
    availableFruits' = sumNames (SLProxy :: SLProxy list)
```

So while we need to apply the classes with quantified variables, these are fully solved and the actual exposed value does not have any free variables.

And as expected:

```hs
    test "SumToList works" do
      Assert.equal
        "Apple, Banana, Cherry"
        (intercalate ", " availableFruits)
```

### `ProductList1` usage

Similarly, we can define a class for the actual `ProductList1` and handle the split information of the constructor name and the actual types.

```hs
-- so we can get the type names of the arguments
class TypeName a where
  typeName :: Proxy a -> String

instance intTypeName :: TypeName (Argument Int) where
  typeName _ = "Int"

instance stringTypeName :: TypeName (Argument String) where
  typeName _ = "String"

-- normal list class like before
class ProductNames (list :: ProductList1) where
  productNames :: PLProxy list -> NonEmptyList String

instance zProductNames ::
  ( TypeName ty
  ) => ProductNames (Product1 ty) where
  productNames _ = pure $ typeName (Proxy :: Proxy ty)

instance sProductNames ::
  ( TypeName ty
  , ProductNames tail
  ) => ProductNames (ProductN ty tail) where
  productNames _ = head <> rest
    where
      head = pure $ typeName (Proxy :: Proxy ty)
      rest = productNames (PLProxy :: PLProxy tail)
```

Then, we can apply the classes as before to prepare the actual static value:

```hs
thingNames :: NonEmptyList String
thingNames = thingNames'
  where
    thingNames'
      :: forall rep name list
      . Generic Thing rep
      => ToProductList rep name list
      => IsSymbol name
      => ProductNames list
      => NonEmptyList String
    thingNames' =
      head <> productNames (PLProxy :: PLProxy list)
      where
        head = pure $ reflectSymbol (SProxy :: SProxy name)
```

Coming together:

```hs
    test "ToProductList/ProductToList works" do
      Assert.equal
        "Thing Int String Int"
        (intercalate " " thingNames)
```

## Conclusion

Hopefully this has shown you that you can readily transform these generics-rep types into other type-level structures as you wish, so you can write instances that require a lot less work than if you tried to work with the original types directly. In addition, you can control which reps you want to actually handle by only writing instances for them -- the `SumToList` does not attempt to transform `Product` reps into `SumList1`, rightly so.

## Links

* This repo https://github.com/justinwoo/purescript-chahan

