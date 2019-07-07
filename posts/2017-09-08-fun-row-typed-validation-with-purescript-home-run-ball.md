# Fun Row-typed Validation with Purescript-Home-Run-Ball

A [while ago](http://qiita.com/kimagure/items/5c248844ab28c8c91b16), I talked about using row types to store validations performed on a string, but I was left feeling like the solution I came up with then wasn't quite there yet.

Recently, I gave a [talk about RowLists](https://www.reddit.com/r/purescript/comments/6xs5f2/rowlist_fun_with_purescript_slides_from_small_fp/) at [Small FP Conf](http://clojutre.org/2017/) and was determined to use RowLists to improve on my previous encodings, which gave rise to [Home-Run-Ball](https://github.com/justinwoo/purescript-home-run-ball).

This library allows you to specify rules to be applied to validate a value against a set of rules and get back a validation, where on success you get back the value with all the validated rules as a Const, and on failure you get back a list of variants, where the tags used are the labels from the rows passed in, with the values being the reflected strings. Let's get to the details.

## Starting with usage

If I want to work with validated values, there's a couple things I already know:

1. I have *names* that I use for what rules I'm applying.
2. I know I don't want to work with a closed set of validation rules: I should be able to use a bunch of rules from a library and also make my own. Using a sum type for this is no good for me.
3. When I get a list of errors back, the possible error names are *statically known* -- anything that doesn't let me handle that explicitly isn't fun.
4. When I get my validated value back, I want to make functions that will then require a subset of the rules that have been applied.

The usage of my library then ends up being the following:

### Declare rules to check in a row proxy

```hs
rules :: RProxy (beginsApple :: BeginsWith "Apple")
rules = RProxy
```

Here we make a row proxy with labels for identifying the rule, with a rule `data BeginsWith :: Symbol -> Type`.

### Create a function that declares what validated rules are required

```hs
onlyOnApples ::
     ValidatedValue (beginsApple :: BeginsWith "Apple") String
  -> String
onlyOnApples _ = "U R COOL"
```

This definition uses the alias `type ValidatedValue (rules :: # Type) a = (...)` to require a validated value with the `beginsApple :: BeginsWith "Apple"` rule from earlier.

### Actually use it!

```hs
-- type VS errors rules a = (...)
validation :: VS (beginsApple :: String) (beginsApple :: BeginsWith "Apple") String
validation = checkRules rules "AppleSDdf"

validation' :: V (NonEmptyList (Variant (beginsApple :: String))) String
validation' = onlyOnApples <$> validation
```

Here we use the function `checkRules` to check an input to our rules and produce a validation `V` with our list of variants of the labels in the rules row for errors and our validated value for the success. We can then use `onlyOnApples` from above accordingly.

## The Details

There are two parts involved here: rule validation and checking of all the rules.

### Validation

Earlier, I showed the definition of `BeginsWith`, but rules can be defined simply as `data Capitalized`. The validation routines are defined by using the `ValidateRule` class:

```hs
class ValidateRule rule a where
  validateRuleImpl :: Proxy rule -> a -> Boolean
```

By taking the proxy in, the type class instance gets solved for `ValidateRule (BeginsWith prefix) String` as defined:

```hs
instance validateRuleBeginsWith ::
  ( IsSymbol prefix
  ) => ValidateRule (BeginsWith prefix) String where
  validateRuleImpl _ str =
    isJust $ stripPrefix (Pattern $ reflectSymbol (SProxy :: SProxy prefix)) str
```

In this case, the prefix gets reflected to be used for attempting to strip the prefix from the input.

### CheckRules

This class drives the application of the rules.

```hs
class CheckRules (rl :: RowList) (errors :: # Type) (rules :: # Type) a
  | rl -> errors rules where
  checkRulesImpl :: RLProxy rl -> a -> V (NonEmptyList (Variant errors)) Unit
```

The `RowList` parameter here comes from the rules passed in, which are technically from `rules`, but `rules` itself can't be used to do instance matching. The errors are produced from the labels in rules reflected into the variant. The final parameter `a` is used to match the type of the input to the `ValidateRule` instance being called.

The method then ends up being the row list proxy being passed in with the value to produce a validation `V` with the variant errors, but with `Unit`, as we don't need to return the value from the instance (and we'll see below why we don't want to).

Let's look at the base case:

```hs
instance checkRulesNil :: CheckRules Nil errors rules a where
  checkRulesImpl _ str = pure unit
```

This instance returns the unit as-is from the method, as no rules can be validated if we have no more rules to apply.

Let's look at the `Cons` instance, first looking only at the constraints:

```hs
instance checkRulesCons ::
  ( IsSymbol name
  , CheckRules tail errors rules a
  , RowCons name String trash errors
  , ValidateRule ty a
  ) => CheckRules (Cons name ty tail) errors rules a where
  checkRulesImpl _ str = (...)
```

So in this instance we declare that the name is a symbol so that we can reflect it in case we have an error. We then check that the rules in the rest of the row list apply to the value. The `RowCons` constraint adds the label with a String value to the errors row (while ignoring what we do with the "subrow"). Finally, the actual rule applying constraint is provided by using `ValidateRule` with the rule type inside our row list along with our validating value's type.

With these constraints, we define the method as follows:

```hs
checkRulesImpl _ str = curr <> rest
    where
      curr
        | validateRuleImpl (Proxy :: Proxy ty) str = pure unit
        | otherwise
        , namep <- SProxy :: SProxy name
        , name <- reflectSymbol namep =
            invalid <<< pure $ inj namep name
      rest = checkRulesImpl (RLProxy :: RLProxy tail) str
```

By using append (`<>`), I'm able to append together the validation errors. If I were to return the validating value inside the validation, I would end up with an unusable mess, so I opted to use `Unit` here since we don't need the result at this point.

### checkRules

To then provide a usable API, I define `checkRules` as so:

```hs
checkRules :: forall a row errors rl
   . RowToList row rl
  => CheckRules rl errors row a
  => RProxy row
  -> a
  -> VS errors row a
checkRules _ str = const (Const str) <$> checkRulesImpl (RLProxy :: RLProxy rl) str
```

From a row proxy of the rules to be applied, we can use `RowToList` to convert that row to the row list we will use, and then further use the `CheckRules` constraint to check the row list and row rules with the input type to produce the error row that is used for the output type. It's like Type Tetris, and that's about it!

## Conclusion

Hopefully this has shown that writing a validation library doesn't have to be too horrible, and that row types let us represent a lot more fun stuff.

Extra thanks to [Hardy (@st58)](https://twitter.com/st58/) for all the help in improving the library and providing a really cool demo [here](https://github.com/joneshf/purescript-home-run-ball-demo) and [Christoph (@kritzcreek)](https://twitter.com/kritzcreek) for the help with walking through the initial version of this library.

If you have any questions about this library, RowList stuff, or anything, please ask me anything on Twitter [@jusrin00](https://twitter.com/jusrin00) or through /r/purescript. Thanks!

## Links

* This repo: https://github.com/justinwoo/purescript-home-run-ball
* Hardy's demo: https://joneshf.github.io/purescript-home-run-ball-demo/, https://github.com/joneshf/purescript-home-run-ball-demo

## おまけ

Checking an Int with your own defined rules is quite easy with this library:

```hs
data Even

instance validateRuleEven :: ValidateRule Even Int where
  validateRuleImpl _ n = mod n 2 == 0

intRules = RProxy :: RProxy (isEven :: Even)

main = -- ...
    it "works with Int too!" do
      let
        checkedNumber = checkRules intRules 4
      isValid checkedNumber `shouldEqual` true
```