Using IxMonad to enforce good hamburger building in Purescript
Recently, I came across a use for indexed monads to do some mutations on a foreign data type and keep track of its actual type as it changed, with a final operation for extracting out the result. While my implementation of this is hacky, there are some cool uses of IxMonad out there that provide useful restrictions, like the way response state is represented in [Hyper](https://github.com/owickstrom/hyper).

For this purpose, I made a demo where you can strictly specify how a hamburger should be built using IxMonad.

## How do you build a burger?

Never mind all the messy cooking bits, the actual building of a burger has multiple stages in my mind:

* Nothing
* An empty plate
* A bottom bun
* A patty
* Some or no cheese
* ...same for lettuce, tomato, and pickles
* A top bun

And we can represent that as data types with no constructors:

```hs
data Ready
data EmptyPlate
data BottomBunOn
data PattyOn
data CheeseOn
data OnionOn
data LettuceOn
data TomatoOn
data PicklesOn
data TopBunOn
```

As for what the ingredients actually are, they can just be anything, so I'll make a newtype for string, with a convenent alias for the list of ingredients that make up the spec of a burger recipe.

```hs
newtype Ingredient = Ingredient String
derive instance newtypeIngredient :: Newtype Ingredient _

type BurgerSpec = List Ingredient
```

## Making our indexed monad

First, we start off with a familiar newtype definition:

```hs
newtype IxBurgerBuilder i o spec = IxBurgerBuilder spec
```

As an *indexed* structure, it needs to have indexing parameters, which we have here as `i` and `o`. The generic `spec` is here to allow just about anything to be shoved into the burger builder, so one could work with not just `List`, but just about any structure you want to shove in.

Next, we'll need a way to extract the spec from our type to use in any other context, like many other IxMonad implementors. This is pretty normal looking:

```hs
runIxBurgerBuilder :: forall prev next spec. IxBurgerBuilder prev next spec -> spec
runIxBurgerBuilder (IxBurgerBuilder spec) = spec
```

The one thing left to do now is to implement IxMonad like so:

```hs
instance ixMonadIxBurgerBuilder :: IxMonad IxBurgerBuilder where
  ipure = IxBurgerBuilder
  ibind (IxBurgerBuilder spec) f = IxBurgerBuilder <<< runIxBurgerBuilder $ f spec
```

`ipure` ends up being a normal `a -> f a`, and the `ibind` definition is not much other than to apply the transformation function to the inner element, extract the value out of the resulting `IxBurgerBuilder` carrying the wrong types, and re-wrap it to have the correct types.

## Adding commands to build our burger

Now that the type machinery is done, we can start writing the most fun part of the application: boilerplate!!! To go from the `Ready` state to the `EmptyPlate` state, we defined a function like so:

```hs
getEmptyPlate :: IxBurgerBuilder Ready EmptyPlate BurgerSpec
getEmptyPlate = IxBurgerBuilder mempty
```

As you can see, the input and output type represent what we wanted to write, and the inner value is our `BurgerSpec` from above.

And just because we don't want to die of boilerplate, we'll define one simple method for adding items to our spec:

```hs
addIngredient :: forall i o. String -> BurgerSpec -> IxBurgerBuilder i o (BurgerSpec)
addIngredient x xs = IxBurgerBuilder $ Ingredient x : xs
```

This way, the `i` and `o` are inferred from the usage, but we can easily define functions as simply `addIngredient "Name"` with the proper type annotations. And so, the body of the commands bloc looks like so:

```hs
-- ADDING THE BUN
placeEmptyBun :: BurgerSpec -> IxBurgerBuilder EmptyPlate BottomBunOn BurgerSpec
placeEmptyBun = addIngredient "Bottom Bun"

-- ADD SOME SAUCES DIRECTLY ON THE BOTTOM BUN
addKetchup :: BurgerSpec -> IxBurgerBuilder BottomBunOn BottomBunOn BurgerSpec
addKetchup = addIngredient "Ketchup"

-- ...

-- PUT THE PATTY ON, NO MORE SAUCES
addPatty :: BurgerSpec -> IxBurgerBuilder BottomBunOn PattyOn BurgerSpec
addPatty = addIngredient "Patty"

-- NEXT IS THE CHEESE, OR NONE
addCheese :: BurgerSpec -> IxBurgerBuilder PattyOn CheeseOn BurgerSpec
addCheese = addIngredient "Cheese"

noCheese :: BurgerSpec -> IxBurgerBuilder PattyOn CheeseOn BurgerSpec
noCheese = IxBurgerBuilder

-- some more definitions later...

-- THEN FINISH THE BURGER WITH THE TOP BUN
addTopBun :: BurgerSpec -> IxBurgerBuilder TomatoOn TopBunOn BurgerSpec
addTopBun = addIngredient "TopBun"
```

And we're done! Once we call `addTopBun`, our state will have been set to `TopBunOn` and we will no longer be able to place anything more on the table with our current commands and index types.

## ハンバーガーをつくりましょ

By using the indexed bind operator `:>>=`, we can write a bunch of these commands for our hamburger spec:

```hs
burgerSpec :: IxBurgerBuilder Ready TopBunOn BurgerSpec
burgerSpec = getEmptyPlate
  :>>= placeEmptyBun
  :>>= addKetchup
  :>>= addPatty
  :>>= addCheese
  :>>= addOnions
  :>>= noLettuce
  :>>= addTomato
  :>>= addTopBun
```

And so, if we print out this spec after running the hamburger builder, we get the following output:

```
My burger consists of:
  TopBun
  Tomato
  Onion
  Cheese
  Patty
  Ketchup
  Bottom Bun
```

Which looks like a pretty correct burger to me!

Let's see an example of an incorrect spec:

```hs
wrongBurgerSpec :: IxBurgerBuilder Ready TopBunOn BurgerSpec
wrongBurgerSpec = getEmptyPlate
  :>>= placeEmptyBun
  :>>= addKetchup
  :>>= addCheese -- Can't match PattyOn with BottomBunOn,
                 --   since we haven't put on the patty,
                 --   the most important part of a burger!!!
  :>>= addOnions
  :>>= noLettuce
  :>>= addTomato
  :>>= addTopBun
```

If we try to add cheese on top of the bottom bun directly, we will get a type error that the `PattyOn` input type we wanted to use didn't match the `BottomBunOn` type, so we can't construct a burger this way. Awesome!

## Takeaway (of digital burgers)

Hopefully this has shown that indexed monads are pretty readily usable to solve problems where you want to restrict operations that can be performed before and after a type has been applied.

If nothing else, hopefully this has shown you one way to properly make hamburgers :-DDD

## Links

* Hyper (https://github.com/owickstrom/hyper) features this kind of approach for working with the status of the response that you can write.

* "roskis"/trashcan demo (https://github.com/justinwoo/purescript-roskis) -- for a more perverse example, you might be interested in my demo where I used an indexed monad to keep track of the type of the mutable record I wrote to, preventing more operations when I pull the record out.

* This repo (https://github.com/justinwoo/hamburger-builder-demo)
