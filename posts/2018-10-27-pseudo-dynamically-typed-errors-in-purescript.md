While we have many ways to work with possible errors in PureScript like catching them in transformers or representing them as an effect of their own in some library, I have found that in many cases, I have wanted to work with the laziest solution many times: shove it in `Error`. And while I would not recommend that people who care about correctness when it comes to errors would use this, many could build layers on top of what I will introduce here with a newtype of `Effect` that tracks the "variety of errors" in an application.

## The Problem

Whether you're working in a team of people who are all very familiar with functional programming (i.e. can comfortably work with monad transformers e.g. `ExceptT`) or you have a team of relatively new PureScript users, you might have some various gripes about working with some kind of `ExceptT MyError Aff` -- primarily, that you have to now handle two layers of errors: one layer which is from the running of the `ExceptT` and one that is from `Error` that are caused by `Effect` and `Aff`.

So if you will handle these errors in some "root" context anyway, why not stuff errors that you want to handle into `Error` and retrieve them later, in operations like `Aff.attempt :: forall a. Aff a -> Aff (Either Error a)`.

But wait, how does one create `Error` anyway? The only function to create `Error` normally seems to be `Effect.Exception.error :: String -> Error`.

## A not-so-good "easy" solution

So many people will see `String` and immediately think "ah ha! I can serialize JSON into that with a concrete type that I will attempt to read out to!" But now you pay excess costs for your errors: you have to try to parse this error JSON string every time into multiple candidates, and you shouldn't really need to encode something just to read it back out if you have a language where you can push more burden of proof into the type system. So how would we do this?

## Say hello to everyone's best friend: class subtyping

Okay, you probably don't actually love subtyping, but this is a good case in which we can use a subtype of `Error` and shove it into `Error` contexts, and there's a fairly low cost to retrieving our subtype in that we can simply run `errorValue instanceof ErrorSubtype` (see <https://en.wikipedia.org/wiki/Liskov_substitution_principle>)

So we can define our subtype and a function to create an error in JS like so:

```js
function VariantError(variant) {
  this.variant = variant;
  this.message = "VariantError";
  return this;
}

VariantError.prototype = Object.create(Error.prototype);
VariantError.prototype.constructor = VariantError;
VariantError.prototype.name = "VariantError";

exports._mkVariantError = function(variant) {
  return new VariantError(variant);
};
```

Awesome! Now we have a `Error` subtype with a `variant` property  that we will write to. And as you might have guessed, we will stuff a `Variant` value into the error, since this lets us be flexible about which "sorts"/variants/members of an error set we can have, so that we don't have to be bound by a (by definition) closed sum type.

*For a quick reminder of what Variant enables: consider that sums and products are complements of each other. We normally work with extensible records in PureScript that are polymorphic for some remaining fields, so what if we have a complement of records that can have one of both a defined and extensible set of values? That is Variant.*

## Our PureScript interface to `VariantError`

So we can write a PureScript interface to this `VariantError` that we have defined, by defining a foreign data type and some related functions that follow the substitution principle.

```hs
foreign import data VariantError :: # Type -> Type

mkVariantError :: forall r. Variant r -> Error
mkVariantError = upcastVariantError <<< _mkVariantError

upcastVariantError :: forall r. VariantError r -> Error
upcastVariantError = unsafeCoerce

foreign import _mkVariantError :: forall a r. a -> VariantError r
```

Amazing! And while we might not use this for very much of our regular PureScript code, having the ability to do this in libraries is quite liberating.

## How do we extract our Variant?

Now let's consider what all we need to do to extract our variant value out of an `Error` to be able to use it.

First, we need to make sure that we check if we have a `VariantError` in our `Error`. We can check for this as mentioned above by using `errorValue instanceof VariantError`, so this part is already done for us.

However, then we need to consider how we even verify that the contained Variant value can be worked with. While we could use Simple-JSON to read the contained value into our type, that's a lot of wasted time for reading out an error, especially considering that we will use these errors inside of our application and they are not distributed by libraries, which would introduce a risk of key collisions. If we can be sure that keys will not collide for the `Variant` values that we work with, we should be able to only check the key. The question is, how should we do this?

## Say hello to everyone's actual best friend: RowToList

Turns out, this crazy guy named Justin has been writing all this shit about `RowToList`, and we can use this to write a type class to reflect the keys of a row type to `String`. Wild, right?

And so, with classic `RowToList` code, we can write a class that can iterate the keys of the row type of a specified `Variant`, so we can check if a given `Variant` value is actually a value we want to handle:

```hs
class MatchKey (r :: # Type) where
  matchKey :: RProxy r -> String -> Boolean

instance matchKeyInst ::
  ( MatchKeyImpl rl
  , RL.RowToList r rl
  ) => MatchKey r where
  matchKey _ = matchKeyImpl (RLProxy :: RLProxy rl)

class MatchKeyImpl (rl :: RL.RowList) where
  matchKeyImpl :: RLProxy rl -> String -> Boolean

instance nilMatchKey :: MatchKeyImpl RL.Nil where
  matchKeyImpl _ _ = false

instance consMatchKey ::
  ( MatchKeyImpl tail
  , IsSymbol name
  ) => MatchKeyImpl (RL.Cons name ty tail) where
  matchKeyImpl _ s = do
    let curr = reflectSymbol (SProxy :: SProxy name)
    if s == curr
       then true
       else matchKeyImpl (RLProxy :: RLProxy tail) s
```

Then we can write some PureScript functions for reading our `Variant` values:

```hs
readVariant :: forall r. MatchKey r => Error -> Maybe (Variant r)
readVariant err =
  _getVariant <$> FU.runFn4
    _readVariantError
    (matchKey (RProxy :: RProxy r))
    Nothing
    Just
    err

getVariant :: forall r. VariantError r -> Variant r
getVariant = _getVariant

foreign import _readVariantError
  :: forall a b r
   . FU.Fn4
       (String -> Boolean)
       (Maybe b)
       (a -> Maybe a)
       Error
       (Maybe (VariantError r))
```

And so, we can accordingly write an implementation in JS that uses this predicate and returns the correct values:

```js
exports._readVariantError = function(test, nothing, just, error) {
  if (error instanceof VariantError) {
    var variant = error.variant;
    if (test(variant.type)) {
      return just(error);
    } else {
      return nothing;
    }
  } else {
    return nothing;
  }
};
```

Nice! Now we have the various pieces we need to work with.

## Example Usage

Say have some row type for possible errors from fetching:

```hs
otherS = SProxy :: SProxy "other"
readForeignS = SProxy :: SProxy "readForeign"

type Error = V.Variant (ErrorRow ())

type ErrorRow r =
  ( other :: EE.Error
  , readForeign :: F.MultipleErrors
  | r
  )
```

Then when we work with a function that will insert `VariantError` into `Error` in `Aff`, we can use guards to get the error from the `Left` constructor when using `Aff.attempt`:

```hs
  -- gives an error in readForeign on mismatched decoding type
  wrong <- Aff.attempt $ fetch' testUrl O.defaultFetchOptions'

  case wrong of
    Left e
      | Just (variant :: O.Error) <- C.readVariant e
      , Just multipleErrors <- V.prj O.readForeignS variant -> do
      pure unit -- success!

    Right (e :: { asdf :: String }) -> Aff.throwError $
      Aff.error "False parsing result"

    Left e -> Aff.throwError e
```

Cool, right? If we couldn't extract the error with the guarded `Left` branch above, then we fall over to handling the generic `Error`, just like we wanted!

## Conclusion

While teams of experienced PureScript users may prefer not to use such an approach like this, working with subtypes of `Error` lets us very easily put together a bunch of `Aff` and other effect functions together without having to deal with the wiring too much, and lets us easily handle the success, known error, and unknown error cases in the same level.

If one still likes this idea but wants to expand it further, they might consider defining an `Eff` like newtype for the errors, which can then be eliminated by using row type constraints to remove elements. Such an idea is more thoroughly explored in the [Checked-Exceptions](https://github.com/natefaubion/purescript-checked-exceptions) library by Nate Faubion.

Otherwise, I hope this gives some insights into other ways in which working with subtypes can be made simple enough in PureScript.

## Links

* This code as a library: <https://github.com/justinwoo/purescript-chirashi>
* Example usage of this library: <https://github.com/justinwoo/purescript-ochadzuke>