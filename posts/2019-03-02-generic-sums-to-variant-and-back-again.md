# Generic Sums to Variant and back again

*Before we start, I should remind you that I've written a Generics-Rep (Datatype Generics) tutorial in the Simple-JSON docs here: <https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html>. I've also written a tutorial on how to work with inferred types with constraints here: <https://purescript-simple-json.readthedocs.io/en/latest/inferred-record-types.html>. These should make for useful concrete background reading which make the ideas in this post flow naturally.*

*You can also learn about what Variants are and how polymorphic variants work from the README in the Variant library here: <https://github.com/natefaubion/purescript-variant>*

## Background

One problem I run into fairly often in PureScript is that I often want to work with plain old sum types, but I need to have them transported via JSON. While sum types don't have a natural representation in JSON, Variants (as in polymorphic variants) do, with the most plain representation being that of a record, of a "type" field and a "value" field. And so, I consider the basically equivalent forms:

```hs
data Fruit = Apple | Banana Int | Kiwi String

type Fruity = Variant
  ( "Apple" :: {}
  , "Banana" :: Int
  , "Kiwi" :: String
  )
```

I've made some concrete decisions from the start, of course, in that since my objective is to reach some simple JSON-compatible representation. This means that...

* Constructors with no arguments take an empty record argument in the Variant form
* I do not allow sum types with multiple arguments (because as products, they can be represented as records)

## Demonstration

And so with this, I implemented a `genericSumToVariant` function:

```hs
data Fruit = Apple | Banana Int | Kiwi String
derive instance genericFruit :: Generic Fruit _

main :: Effect Unit
main = do
  logShow $ genericSumToVariant Apple
  logShow $ genericSumToVariant (Banana 3)
  logShow $ genericSumToVariant (Kiwi "green")

  -- results:
  -- (inj @"Apple" {})
  -- (inj @"Banana" 3)
  -- (inj @"Kiwi" "green")

  log $ JSON.writeJSON $ genericSumToVariant Apple
  log $ JSON.writeJSON $ genericSumToVariant (Banana 3)
  log $ JSON.writeJSON $ genericSumToVariant (Kiwi "green")

  -- results:
  -- {"type":"Apple","value":{}}
  -- {"type":"Banana","value":3}
  -- {"type":"Kiwi","value":"green"}
```

That's not all either, as we can also get the reverse with `variantToGenericSum`:

```hs
  -- converts the other way around too, given a fixed sum type
  assertEqual
    { expected: Apple
    , actual: variantToGenericSum (Variant.inj (SProxy :: _ "Apple") {})
    }

  let testJSON1 = """{"type":"Apple","value":{}}"""
  assertEqual
    { expected: Right Apple
    , actual: variantToGenericSum <$> JSON.readJSON testJSON1
    }
```

Amazing! Now let's dig into how this works.

## Methods

If you're familiar enough with working with Generics-Rep, you can probably guess that I use it to convert from and to the generic forms, and so the top level functions are a wrapper around the implementation that provide for some constraints to be applied.

```hs
-- | Convert a generic sum into a variant
genericSumToVariant :: forall a rep r
   . GR.Generic a rep => GenericSumToVariant rep r
  => a -> Variant r
genericSumToVariant a = genericSumToVariantImpl (GR.from a)

-- | Convert a variant into a generic sum, where the type of the sum determines
-- | the type of the variant, because you could not construct the sum otherwise.
-- | This also means that we can recover from the alternative introduced when
-- | expanding and contracting the Variant rows.
variantToGenericSum :: forall a rep r
   . GR.Generic a rep => GenericSumToVariant rep r
  => Variant r -> a
variantToGenericSum v =
  case GR.to <$> (variantImplToGenericSumImpl v) of
    -- ...
```

### `GenericSumToVariant`

Given our constraints that we only deal with sum types and their constructors, our type class for solving out the row type of a Variant with only instances for those reps. This also means that our class is only able to solve for the row using the representation, meaning that we must always provide a concrete sum type to do our transformation -- which is fair, because how would an anonymous sum become identified? This blog is about programming, not philosophy.

```hs
class GenericSumToVariant rep (r :: # Type) | rep -> r where
  genericSumToVariantImpl :: rep -> Variant r
  variantImplToGenericSumImpl :: Variant r -> Maybe rep
```

The `Maybe` is here because the conversion of the variant to a rep is akin to decoding, so there are many failure cases within the branches of the solution. However, we know that given a concrete solution of the whole, we will not have a failing branch. We could encode this information in the type class by adding even more parameters, but I chose not to overload the kind signature with our petty details.

### The constructor case

Let's start with the base case: when we have a constructor of our sum type. In this branch, we need to provide for a single member of the row type, which we can build by constructing a row from an empty one:

```hs
instance genericSumToVariantConstructor ::
  ( IsSymbol name
  , Row.Cons name ty () r
  , GenericSumToVariantArg a ty
  ) => GenericSumToVariant (GR.Constructor name a) r where
```

And these constraints are enough, that declare a label at which a type exists in a Variant singleton. The other class `GenericSumToVariantArg` handles the conversion of the value, where `NoArguments` will be converted to `{}` and any other argument unwrapped.

And so the implementation for sum to variant is a matter of constructing the variant value, and the variant to sum is to attempt reading the value at the label to return a constructed rep.

```hs
instance genericSumToVariantConstructor ::
  ( IsSymbol name
  , Row.Cons name ty () r
  , GenericSumToVariantArg a ty
  ) => GenericSumToVariant (GR.Constructor name a) r where
  genericSumToVariantImpl (GR.Constructor a) =
    Variant.inj nameS value
    where
      nameS = SProxy :: _ name
      value = genericSumToVariantArgImpl a
  variantImplToGenericSumImpl v = do
    x :: ty <- Variant.prj nameS v
    Just $ GR.Constructor $ variantArgImplToGenericSumImpl x
    where
      nameS = SProxy :: _ name
```

### The sum case

For the sum case, we need to put together the solution that comes from the left and the right sides, creating a union of the solutions. For the sum to variant case, we need the following constraints:

* Solve the left side subrow
* Solve the right side subrow
* Solve the union of the left and right sides
* Solve that the left is a subset of the total union
* Solve that the right is a subset of the total union

i.e. for `a + b = c`, `c - b > 0` and `c - a > 0`

However, for the variant to sum case, we will also need to provide some extra constraints so that we can constrain our variant: we need to be able to shrink the total variant into the subrow variant that our branch can solve for. This involves some extra constraints from Variant.

```hs
instance genericSumToVariantSum ::
  ( GenericSumToVariant a ra
  , GenericSumToVariant b rb
  , Row.Union ra ra' r
  , Row.Union rb rb' r
  , Row.Union ra rb r
  -- For Variant
  , RL.RowToList ra raL
  , VariantTags raL
  , RL.RowToList rb rbL
  , VariantTags rbL
  ) => GenericSumToVariant (GR.Sum a b) r where

  -- for a subrow case, expand the solution to the whole
  genericSumToVariantImpl (GR.Inl a) = Variant.expand (genericSumToVariantImpl a :: Variant ra)
  genericSumToVariantImpl (GR.Inr b) = Variant.expand (genericSumToVariantImpl b :: Variant rb)

  -- contract the whole into the solvable parts, and provide the alternative of the two
  variantImplToGenericSumImpl v = do
    readLeft <|> readRight
    where
      readLeft = do
        v' :: Variant ra  <- Variant.contract v
        gs :: a <- variantImplToGenericSumImpl v'
        Just (GR.Inl gs)
      readRight = do
        v' :: Variant rb  <- Variant.contract v
        gs :: b <- variantImplToGenericSumImpl v'
        Just (GR.Inr gs)
```

And this is about it. By having the sum solve the top level where the work is divided into the sub parts, we solve the whole system.

## Conclusion

Hopefully this has shown you that by building a bit on top of the knowledge from the tutorials I linked, one can build a solution for solving some concrete problems by leveraging information about types generically using constraints, i.e. getting computers to do their damn job.

## Links

* This library <https://github.com/justinwoo/purescript-kishimen>
* Generics-Rep tutorial <https://purescript-simple-json.readthedocs.io/en/latest/generics-rep.html>
* Inferred types and constraints tutorial <https://purescript-simple-json.readthedocs.io/en/latest/inferred-record-types.html>
* Variant <https://github.com/natefaubion/purescript-variant>