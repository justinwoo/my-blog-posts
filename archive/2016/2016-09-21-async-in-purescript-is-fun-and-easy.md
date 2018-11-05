# Async in Purescript is fun and easy

This post is targeted to people who are getting started or are interested in Purescript. There are some parallels with things that exist in Javascript, but not really, since there are no real facilities to make such generic interfaces possible in Javascript.

Let's get started.

## Callbacks with Eff

This is probably the most common thing you'll need done -- the resolution of a single value. The simplest way to do this is to use a callback, so you can have a value computed and have your program continue when that value is ready.

It's more than likely that you'll be using some existing Javascript libraries that take callbacks for async. For each `{name}.purs` file you have, a corresponding `{name}.js` file can be used for foreign functions that you want to use. Like so:

```js
// Main.js
exports.foreignCalculateLength = function(array) {
  return function (callback) {
    return callback(array.length);
  };
};
```

```haskell
foreign import foreignCalculateLength :: forall e a.
  Array a ->
  (Int -> Eff e Unit) ->
  Eff e Unit
```

So you can see that our `foreignCalculateLength` function takes an `Array a` to return a function that will then take a function that takes `Int` to return an `Eff`ect of `Unit` (basically nothing). Registering a callback returns `Eff e Unit`, which means that we need to run this somewhere that can "unwrap" `Eff` to pull out our `(gooey burrito guts | insert your metaphor here)`.

This can then be used in a main function:

```haskell
type Main e = Eff (console :: CONSOLE | e) Unit

mainCallback :: forall e. Main e
mainCallback =
  foreignCalculateLength [1,2,3] (\x -> do
    logShow (x * 2) -- prints "6"
  )
```

We've calculated the length of our array and passed in a callback to display that length times two!

We can write this Eff callback in pure Purescript also.

```haskell
calculateLengthEff :: forall e a.
  Array a ->
  (Int -> Eff e Unit) ->
  Eff e Unit
calculateLengthEff l s = do
  s (length l)
  pure unit
```

Note: You'll see that we are ending our `do` block with `pure unit`, which matches the return type of `Eff e Unit`. This is because `Eff` has an [instance](https://github.com/purescript/purescript-eff/blob/541af8efb56da15435ae4d8be5c890cff81d3878/src/Control/Monad/Eff.purs#L32-L33) of the [`Applicative`](https://pursuit.purescript.org/packages/purescript-prelude/1.1.0/docs/Control.Applicative#t:Applicative) typeclass, which means that the member method `pure` of `Applicative` can be used. `pure` is of type `a -> f a`, which means that we can "wrap" any value (in this case, `unit :: Unit` to `Eff`).

And just like in JS, you can run another callback inside of a callback to combine the results later:

```haskell
mainCallback2 :: forall e. Main e
mainCallback2 =
  foreignCalculateLength [1,2,3] (\x -> do
    calculateLengthEff [4,5,6] (\y -> do
      logShow (x + y)
    )
  )
```

We'll learn a better way to do this looking at `Aff`s.

## Asynchronous Effects, or "Affects"

`Aff`s are relatively easy to construct, and may remind you of something you've used in JS, but there are some key differences to note. Let's look at how they work and then talk about it. First, some ways they're made:

1) Converting an existing callback Eff to an Aff:

```haskell
calculateLengthAff :: forall e a. Array a -> Aff e Int
calculateLengthAff l = makeAff \error success -> foreignCalculateLength l success
```

2) Calling the success handler directly:

```haskell
calculateLengthAff' :: forall e a. Array a -> Aff e Int
calculateLengthAff' l = makeAff \error success -> do
  success (length l)
```

3) Using the `Applicative` instance to easily wrap a value in an Aff:

```haskell
calculateLengthAff'' :: forall e a. Array a -> Aff e Int
calculateLengthAff'' l = pure (length l)
```

Sort of like `Eff`s, you can use `do` blocks with `Aff`s, but you have to use `launchAff` to use them. Like so:

```haskell
type MainAff e = Eff (err :: EXCEPTION, console :: CONSOLE | e) (Canceler (console :: CONSOLE | e))

mainAff :: forall e. MainAff e
mainAff = launchAff do
  x <- calculateLengthAff [1,2,3]
  liftEff' (logShow (x * 2))
```

You'll see that `launchAff` returns a `Canceler`, which can be used to cancel `Aff`s if needed. This can become useful in cancelling HTTP requests and other things, but isn't too important for our uses right now.

Remember that example before where we needed to run callbacks in succession? With `Aff`s, that becomes really easy:

```haskell
mainAff2 :: forall e. MainAff e
mainAff2 = launchAff do
  x <- calculateLengthAff [1,2,3]
  y <- calculateLengthAff' [4,5]
  z <- calculateLengthAff' [6]
  liftEff' (logShow (x + y + z))
```

And the one where we want to multiply the result by 2? Sure, we could multiply the value where we log the value, but what if we wanted to make it part of the pipeline that could be reused? No problem, as we can use the [Functor](https://pursuit.purescript.org/packages/purescript-prelude/1.1.0/docs/Data.Functor#t:Functor) instance to `map/<$>` of the type `(a -> b) -> f a -> f b`.

```haskell
mainAff3 :: forall e. MainAff e
mainAff3 = launchAff do
  result <- (*) 2 <$> (calculateLengthAff [1,2,3])
  liftEff' (logShow result) -- prints "6"
```

What about if we wanted to take the three successive `Aff`s and... run them in parallel? That we can, since `Aff` has an instance of [MonadPar](https://pursuit.purescript.org/packages/purescript-parallel/1.1.0/docs/Control.Parallel.Class#t:MonadPar) from purescript-parallel, which in turn has an instance of [Apply](https://pursuit.purescript.org/packages/purescript-prelude/1.1.0/docs/Control.Apply#t:Apply), which lets us put these parallel processes together using `apply/<*>`.

```haskell
mainAff4 :: forall e. MainAff e
mainAff4 = launchAff do
  result <- runParallel $ (\a b c -> a + b + c)
    <$> parallel (calculateLengthAff [1,2,3])
    <*> parallel (calculateLengthAff' [4,5])
    <*> parallel (calculateLengthAff'' [6])
  liftEff' (logShow result)
```

So we can see that doing things that are commonly thought to be hard is actually fairly easy with Purescript thanks to the common interfaces available.

In the majority of your Purescript applications and the libraries you use, `Aff` is the answer for single-value async that allows you to write very simple code using `do` blocks.

*Aside: you have probably noticed that `Aff`s remind you of Promises in Javascript. Unfortunately, Promises are eager and cannot be reused in the same way\*. They also expose their own specific methods like most Javascript objects, so there is no way to generically transform Promises in a similar fashion as with Arrays or other structures. Lastly, they do not currently provide any method for cancellation, though there are proposals to dramatically change Javascript and Promises themselves to support cancellation that may land in the following years on newer environments.*

*\* for example, you could not do this with Promises:*

```haskell
mainAff5 :: forall e. MainAff e
mainAff5 = launchAff do
  let aff = calculateLengthAff [1,2,3]
  result <- runParallel $ (\x y z -> x + y + z)
    <$> parallel aff
    <*> parallel ((_ - 1) <$> aff)
    <*> parallel ((_ - 2) <$> aff)
  liftEff' (logShow result) -- prints "6"
```

## Multiple async values with Observables

This is an area in which there are multiple solutions, but my favorite libraries for 0-N async values are reactive programming libraries, especially observable libraries.

Like `Aff`s and other structures, Observable libraries have three stages that the user cares about: 1) Creation (creating an Observable), 2) Subscription (subscribing to an Observable), 3) Disposal (disposing of a subscription). Of course, the first two cases are the most important.

[Purescript-observable](https://github.com/bodil/purescript-observable/) is one such library for this in Purescript.

Now, there isn't a super straight-forward way to convert an `Aff` into an `Observable`, so I made a small [library](https://github.com/justinwoo/purescript-observable-lift) for this. Using this library, we can create `Observable`s in a similar fashion as with `Aff`s:

```haskell
calculateLengthObs :: forall e a. Array a -> Eff (observable :: OBSERVABLE | e) (Observable Int)
calculateLengthObs l = liftAff (calculateLengthAff l)

calculateLengthObs' :: forall e a. Array a -> Eff (observable :: OBSERVABLE | e) (Observable Int)
calculateLengthObs' l = observable \sink -> do
  sink.next (length l)
  sink.complete
  free [] -- for returning inner subscriptions that must be disposed when this observable is disposed.

calculateLengthObs'' :: forall a. Array a -> Observable Int
calculateLengthObs'' l = pure (length l)
```

And then we just apply what is basically the same code as with `Aff`s:

```haskell
type MainObs e = Eff (observable :: OBSERVABLE, console :: CONSOLE | e) (Subscription (console :: CONSOLE | e))

mainObs :: forall e. MainObs e
mainObs = do
  s <- calculateLengthObs [1,2,3]
  subscribe
    { next : \x -> logShow (x * 2)
    , error: message >>> logShow
    , complete: pure unit
    }
    s

mainObs2 :: forall e. MainObs e
mainObs2 = do
  s <- calculateLengthObs [1,2,3]
  subscribe
    { next : logShow
    , error: message >>> logShow
    , complete: pure unit
    }
    ((*) 2 <$> s)

mainObs3 :: forall e. MainObs e
mainObs3 = do
  s1 <- calculateLengthObs [1,2,3]
  s2 <- calculateLengthObs' [4,5]
  let s3 = calculateLengthObs'' [6]
  subscribe
    { next: logShow
    , error: message >>> logShow
    , complete: pure unit
    }
    ((\x y z -> x + y + z) <$> s1 <*> s2 <*> s3)
```

Note that with Observables, you have three callbacks you must provide. `next` is for handling each emitted value; `error` is for handling each emitted error; `complete` is what must be run after the Observable has signaled that it has completed and has no more values to emit. But otherwise, the ideas are largely the same.

I even wrote a wrapper library around another Observable stream library called [`purescript-xstream`](https://github.com/justinwoo/purescript-xstream) (the original `xstream` written by a friend who has a love for incredibly cheesy names) which has a similar API with some more things for convenience:

```haskell
calculateLengthXStream :: forall e a. Array a -> Eff (stream :: STREAM, ref :: REF | e) (Stream Int)
calculateLengthXStream l = fromAff (calculateLengthAff l)

calculateLengthXStream' :: forall a e. Array a -> Eff (stream :: STREAM | e) (Stream Int)
calculateLengthXStream' l =
  create
    { start: \o -> do
        o.next (length l)
        o.complete unit
    , stop: const (pure unit)
    }

mainXStream :: forall e.
  Eff
    ( stream :: STREAM
    , ref :: REF
    , console :: CONSOLE
    | e
    )
    Unit
mainXStream = do
  s1 <- calculateLengthXStream [1,2,3]
  s2 <- calculateLengthXStream' [4,5]
  addListener
    { next: logShow
    , error: message >>> logShow
    , complete: const (pure unit)
    }
    ((+) <$> s1 <*> ((+) 1 <$> s2))
```

## Conclusion

So I hope this has covered the most common async cases that you will run into when using Purescript and that you've been convinced this is pretty fun and fairly easy (and maybe even cool, *for some definitions of cool*).

If you read this far, thanks for reading! Please let me know if you have any corrections/critiques/comments/reactions/responses/hatemail/concerns/praises/suggestions on [twitter](https://twitter.com/jusrin00/).

Thanks!

## Links

* Code for this post: https://github.com/justinwoo/purescript-async-is-easy
* Purescript by Example: https://leanpub.com/purescript/read
* purescript-aff: https://github.com/slamdata/purescript-aff
* purescript-observable: https://github.com/bodil/purescript-observable
* purescript-xstream: https://github.com/justinwoo/purescript-xstream

