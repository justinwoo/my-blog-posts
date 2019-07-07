# Nice Validation with PureScript

Edit: I updated the usage of Eff to Effect to account for updates to PureScript, but everything else here is largely the same.

In many programs that we write, we have the need to validate data before we perform any more operations. Unfortunately, most of time we end up with a solution that requires us to build a list of errors and check this list's contents for being empty, but there are easier and nicer solutions to this problem. This is where PureScript's Validation library comes into play.

## What is Validation?

```hs
data V err result
```

The `Validation` package gives us a nice way to validate values using applicative validation. From the docs:

> Applicative validation differs from monadic validation using `Either` in that it allows us to collect multiple errors using a `Semigroup`, whereas `Either` terminates on the first error.

In other words, modeling validations with `Either` would fall out on the first error. We can collect one or more errors in this case using a structure that is an instance of `Semigroup`.

Note that this does not work with `Monoid`. Using `Monoid` would not make sense in that it would provide an identity that cannot reasonably be used—there is no case in which you would have an empty list of errors and fall on the error case of the validation.

## Record validation example

Say we had some form data represented in a record:

```hs
type FormData =
  { appleColor :: String
  , bananaColor :: String
  , carrotColor :: String
  }
```

So we will be validating the colors of these fruits. Let's define a type alias that we will be using for our validations.

```hs
type MyValidated a = V (NonEmptyList String) a
```

Now let's define some functions that will perform validation.

```hs
appleIsRed :: String -> MyValidated String
appleIsRed s =
  if s == "red"
     -- creates our MyValidated with the right side Valid String
     then pure s
     -- creates our MyValidated with the left side InValid (NonEmptyList String)
     else invalid $ pure "apple wasn't red"

bananaIsNotGreen :: String -> MyValidated String
bananaIsNotGreen s =
  if s /= "green"
     then pure s
     else invalid $ pure "banana was green"
```

Now we can use these validations to create a value of `MyValidated`.

```hs
testData :: FormData
testData =
  { appleColor: "red"
  , bananaColor: "yellow"
  , carrotColor: "orange"
  }

testMyValidated :: MyValidated FormData
testMyValidated =
  { appleColor: _, bananaColor: _, carrotColor: testData.carrotColor }
  <$> appleIsRed testData.appleColor
  <*> bananaIsNotGreen testData.bananaColor
```

And we can use this validated value already.

```hs
printMyValidated :: MyValidated FormData -> Effect Unit
printMyValidated = unV
  (\errors -> log $ "got errors: " <> intercalate ", " errors)
  (\formData ->
    log
      $ "the apples were "
      <> formData.appleColor
      <> " and the bananas "
      <> formData.bananaColor
  )

main :: Effect Unit
main = do
  printMyValidated testMyValidated
  -- output:
  -- the apples were red and the bananas yellow
```

And it works! Let's look at what happens with the error cases.

```hs
errorMyValidated1 :: MyValidated FormData
errorMyValidated1 =
  { appleColor: _, bananaColor: _, carrotColor: testData.carrotColor }
  <$> appleIsRed "red"
  <*> bananaIsNotGreen "green"

errorMyValidated2 :: MyValidated FormData
errorMyValidated2 =
  { appleColor: _, bananaColor: _, carrotColor: testData.carrotColor }
  <$> appleIsRed "yellow"
  <*> bananaIsNotGreen "green"

main :: Effect Unit
main = do
  printMyValidated testMyValidated
  -- output:
  -- the apples were red and the bananas yellow

  printMyValidated errorMyValidated1
  printMyValidated errorMyValidated2
  -- output:
  -- got errors: banana was green
  -- got errors: apple wasn't red, banana was green
```

And so we can see that in the case of errors, the errors are accumulated on the left side and presented to us when we run `unV`.

## More interesting uses of Validation

I used the `Validation` library as the basis for my [Home-Run-Ball library](https://github.com/justinwoo/purescript-home-run-ball). In this library, I wrote a type class `CheckRules` which returns a `V (NonEmptyList (Variant errors)) Unit`, for which I have an instance which appends together multiples of these errors like so:

```hs
class CheckRules (rl :: RowList) (errors :: # Type) (rules :: # Type) a
  | rl -> errors rules where
  checkRulesImpl :: RLProxy rl -> a -> V (NonEmptyList (Variant errors)) Unit

instance checkRulesCons ::
  ( -- [...]
  ) => CheckRules (Cons name ty tail) errors rules a where
  checkRulesImpl _ str = curr <> rest
    where
      -- [...]

instance checkRulesNil :: CheckRules Nil errors rules a where
  checkRulesImpl _ str = pure unit
```

This way, I had static information about which errors could occur from my row type of validations to be run. I was then able to use the `Functor` instance to return the value with evidence of validations I had run:

```hs
-- | Type alias for a validated string and its rules
type ValidatedValue (rules :: # Type) a = Const a (RProxy rules)

-- | Type alias for a string validation result,
-- | with a list of labels that failed validation
type VS errors rules a =
  V (NonEmptyList (Variant errors)) (ValidatedValue rules a)

-- | Check a string for validation rules provided by a row proxy
-- | and return a validation result
checkRules :: forall a row errors rl
   . RowToList row rl
  => CheckRules rl errors row a
  => RProxy row
  -> a
  -> VS errors row a
checkRules _ str =
  const (Const str) <$> checkRulesImpl (RLProxy :: RLProxy rl) str
```

So out of the relatively "simple" idea of applicative validation, I was able to create a library on top that provides further power in modeling correct code paths. I wrote about this more [here](https://qiita.com/kimagure/items/eeb40541fc56b8dba2cc), so please check it out if you're interested.

## Conclusion

I hope this post was able to show that the applicative `Validation` library in PureScript is pretty nice and allows us to express a validation problem we commonly have using safe and nicely statically defined methods. Instead of requiring us to do any kind of runtime checks on a list of errors as we might have manually written, this gives us a nice static way to work with errors that may come up in our program.

## Links

* Code for this post: https://github.com/justinwoo/nice-validation-example
* Validation: https://github.com/purescript/purescript-validation