One of the most fun and interesting parts of Purescript have to do with its row types, which are typically used for two things:

* Records -- for encoding Javascript-style objects directly
* Effects -- for encoding effects of a given `Eff` operation

We also get access to these row types to define just about anything we want, and so, we can even create a row type for tracking validations performed on a string!

## Kinds and data types

First, we need to declare a kind to be used for our rows.

```hs
foreign import kind CONDITION
```

Now that we have `CONDITION`, we can declare a data type for it. We're only really interested in validations, so we only need "yes, this has been validated".

```hs
foreign import data YES :: CONDITION
```

We can now define our data type which uses rows of `CONDITION`. We use the operator `#` to create rows of a kind.

```hs
foreign import data Validations :: # CONDITION -> Type
```

This is it for the fancy type stuff! Now we can get to cooking with gas!

## Preparing our stove

Let's define some handy type aliases to work with:

```hs
type E a = Either String a
type C v = Const String (Validations v)
```

`Const` is defined as `newtype Const a b = Const a`, which makes it incredibly useful for exposing the first type argument as a value while ignoring the second. We use this second parameter to store our Validations type.

If you've run Hello World with Purescript, you'll probably have used and seen this function:

```hs
log :: forall eff. String -> Eff (console :: CONSOLE | eff) Unit
```

Matching what you know from the above section, you'll probably guess that `Eff` is defined `data Eff :: # Effect -> Type -> Type`. With the rows of `Effect`, you can extend the row to include rows as you like, which is the case here with `log` adding a `console :: CONSOLE` row to our effects. We'll be using this same thing onward.

## Writing our Validations

The first validation we want to write is if our string "starts" with a tag. Its signature is

```hs
startsWithTag :: forall v.
  C v
  -> E (C (startsWithTag :: YES | v))
```

So it takes a string with existing validations and returns an either for our string with `startsWithTag :: YES` added to the validations. In the case the validation fails, it'll return the left side string error message. In our implementation, we have no mention of our validation types since they won't be needed:

```hs
startsWithTag (Const s)
  | Just a <- indexOf (Pattern "[") s
  , Just b <- indexOf (Pattern "]") s
  , a < b = Right (Const s)
  | otherwise = Left "where's the tag"
```

Here we can use the guard syntax to provide a sequence of bindings and conditions such that any failure of a binding or condition will continue to the next candidates. In the case that our validation to look for a "[" and "]" indices and compare them has passed, we'll return the right side const string, which will then have the additional row added to its validations.

Our other validations look pretty much the same, adding new rows of their own with different labels:

```hs
containsEpisodeNumber :: forall v.
  C v
  -> E (C (containsEpisodeNumber :: YES | v))
containsEpisodeNumber (Const s)
  | [_, a] <- split (Pattern " - ") s
  , [b, _] <- split (Pattern ".") a
  , c <- toCharArray b
  , upper <- (>=) (toCharCode '9')
  , lower <- (<=) (toCharCode '0')
  , check <- conj upper lower
  , test <- and $ check <<< toCharCode <$> c
  , test == true = Right (Const s)
  | otherwise = Left "where's the episode number"

endsWithExtension :: forall v.
  C v
  -> E (C (endsWithExtension :: YES | v))
endsWithExtension (Const s)
  | [_, a] <- split (Pattern " - ") s
  , [_, b] <- split (Pattern ".") a
  , mkv <- (==) "mkv"
  , avi <- (==) "avi"
  , mp4 <- (==) "mp4"
  , test <- or [mkv, avi, mp4] b
  , test == true = Right (Const s)
  | otherwise = Left "where's the extension"
```

## Writing our function requiring validations

Our function requiring validations looks like the opposite of our validation functions:

```hs
action :: forall e v.
  C
    ( startsWithTag :: YES
    , containsEpisodeNumber :: YES
    , endsWithExtension :: YES
    | v
    )
  -> Eff (console :: CONSOLE | e) Unit
action (Const s) = log $ "yay, we validated " <> s
```

This definition ensures that in compile time, this function can only be called if the const string has the validations specified. Importantly, the rows are unordered, so we can run our validations in any order (otherwise, it'd be a little bit annoying to then reuse these validations in other functions or use our validated string on multiple actions requiring different validations).

One thing we should note though, is that the validations can be inferred incorrectly when we try to use them. Understandably so, since the inference assumes that you have meant to do so and have already validated it. To prevent this, we need a small helper to prevent that inference:

```hs
mkConst :: String -> C ()
mkConst = Const
```

This makes sure that we're explicitly working with an empty set of validations for a new const string.

## Putting this to work

Now we can put this to work:

```hs
main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  traverse_ go
    [ "Wheresthetag - 01.mkv"
    , "[Crap] WrongFileExtension - 01.app"
    , "[Crap] NoEp numer.mkv"
    , "[Crap] ABCDEF - 01.mkv"
    ]
  where
    go str = do
      case endsWithExtension
        =<< startsWithTag
        =<< containsEpisodeNumber (mkConst str) of
        Right s -> action s
        Left e -> log $ show e <> " in " <> str
```

This will print out the following:

```
"where's the tag" in Wheresthetag - 01.mkv
"where's the extension" in [Crap] WrongFileExtension - 01.app
"where's the episode number" in [Crap] NoEp numer.mkv
yay, we validated [Crap] ABCDEF - 01.mkv
```

You might think this is pretty silly, since this is what we'd expect anyway. But if we take away one of the validations, we can confirm this checks as we wanted:

```hs
    -- this doesn't work, yay!!
    go2 str =
      case endsWithExtension
        =<< containsEpisodeNumber (mkConst str) of
        Right s -> action s -- errors because startsWithTag validation is missing!
        Left e -> log $ show e <> " in " <> str
```

and the error is fairly descriptive:

```
  Could not match type

    ( containsEpisodeNumber :: YES
    , endsWithExtension :: YES
    )

  with type

    ( startsWithTag :: YES
    , containsEpisodeNumber :: YES
    , endsWithExtension :: YES
    | t0
    )

while trying to match type Validations
                             ( endsWithExtension :: YES
                             , containsEpisodeNumber :: YES
                             )
  with type Validations
              ( startsWithTag :: YES
              , containsEpisodeNumber :: YES
              , endsWithExtension :: YES
              | t0
              )
while checking that expression s
  has type Const String
             (Validations
                ( startsWithTag :: YES
                , containsEpisodeNumber :: YES
                , endsWithExtension :: YES
                | t0
                )
             )
```

## Conclusion

Hopefully this has shown you that we can use row types for fun like storing information about unordered validation of values and to define functions that use them.

## Links

* Repo https://github.com/justinwoo/purescript-const-string-validation-demo/blob/master/src/Main.purs
* Documentation about Rows https://github.com/purescript/documentation/blob/master/language/Types.md#rows