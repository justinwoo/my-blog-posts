When using PureScript, one of the main features is being able to use the foreign function interface for directly calling into JavaScript to get the results you need. This has many benefits, such as

* Being able to give proper types to a given expression
* Not being limited by runtime implementations for when certain kinds of operations can be performed, especially concerning synchronous vs asynchronous, privileged callbacks, etc.
* Being able to build on top of the FFI imports
* Being able to do things some implementer/"thoughtleader" hasn't thought of doing

So instead of getting stuck when someone can't find an existing solution for something that requires calls to JavaScript that a) exists, b) is useful for them, or c) that works well enough, it's much more useful to enable them to help themselves. And so, I hope to provide some real examples in this post with some links to resources where people can actually learn more.

## Working with Puppeteer

 [Puppeteer](https://github.com/GoogleChrome/puppeteer) is a Node library for working with Chrome/Chromium over the DevTools protocol, so that you can automate browser actions for doing things like poking around at your application, scraping image search results, and other things.

 To work with Puppeteer with a sensible typed interface in PureScript, I made the library [Toppokki](https://github.com/justinwoo/purescript-toppokki). For example, to have Puppeteer open `example.com`, test that the content is not empty, and take a screenshot and PDF, I can work with the following code:

```hs
import Toppokki as T
main = launchAff_ do
  browser <- T.launch

  page <- T.newPage browser
  T.goto (T.URL "https://example.com") page

  content <- T.content page
  Assert.assert "content is non-empty string" (String.length content > 0)

  _ <- T.screenshot {path: "./test/test.png"} page
  _ <- T.pdf {path: "./test/test.pdf"} page

  T.close browser
```

To implement this library or something similar in your codebase, you really only need to see these three resources:

* PureScript language FFI documentation <https://github.com/purescript/documentation/blob/master/language/FFI.md>
* PureScript documentation guide on FFI <https://github.com/purescript/documentation/blob/master/guides/FFI.md>
* PureScript-Effect uncurried function documentation <https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Uncurried>

Primarily, I'll cover three main things that need to be known to work with FFI effectively in PureScript:

* Declaring a foreign data type
* Uncurried pure and Effect functions
* Aff-Promise to thunk Promises and use Aff

## Declaring a foreign data type

If you have some value that should be typed but cannot be created from PureScript other than by FFI, you should be declaring a foreign data type. The `Browser` type in Toppokki is defined in this way:

```hs
foreign import data Browser :: Type
```

### `:: Type`

You might ask, "what is `:: Type`"? This is exactly like Type signatures of values you may have, like `1 :: Int` or `"string" :: String` but for types, known as a Kind signature. These are "types of types", and by giving a type the kind signature `Type`, we are declaring that there exist values of this as `Type`s are representable in the runtime.

For an example, see the documentation of [Array](https://pursuit.purescript.org/builtins/docs/Prim#t:Array), which has the signature

```hs
data Array :: Type -> Type
```

As a `Type -> Type`, this does not have a valid value. But by applying a `Type` argument, we can get a real type:

```hs
type ArrayInt = Array Int

aint = [1] :: ArrayInt
```

For further reading, see <https://github.com/purescript/documentation/blob/master/language/Types.md#kind-system> and <https://leanpub.com/purescript/read#leanpub-auto-type-constructors-and-kinds>. Once you have read those sections, you should look at the definition of `SProxy` and think about what it means: <https://pursuit.purescript.org/packages/purescript-prelude/4.1.0/docs/Data.Symbol#t:SProxy>

## Uncurried Effect functions

This section is better explained in the docs for Effect.Uncurried here: <https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Uncurried>

As you might know, a typical PureScript function is actually a series of functions based on the number of arguments they take. For example, `myFn :: Int -> String -> Boolean` will give you a function `function myFn (int) { return function (str) { return boolean }}`, so this function is to be called `myFn(1)("str")` from JavaScript. A function of `Effect` is thunked, so an extra function surrounds the return value. For instance, `function (i) { console.log(i) }` would immediately cause an effect when the Int is supplied, so it is instead represented as `function effFn (i) { return function () { console.log(i) }}` and typed `effFn :: Int -> Effect Unit`.

Then, to define normal effectful functions with multiple arguments, we use functions from `Effect.Uncurried` and then use the `runEffectFn_` functions to run these `EffectFn_` functions, and `mkEffectFn_` functions to create them. For example,

```js
exports._on = function(event, callback, page) {
  return page.on(event, callback);
};
```

`on_` takes three args here and causes an effect, so we foreign import this as

```hs
foreign import _on :: forall a. EU.EffectFn3 String (EU.EffectFn1 a Unit) Page Unit
```

With this, we can then make normal PureScript function that has curried arguments:


```hs
onLoad :: EU.EffectFn1 Unit Unit -> Page -> Effect Unit
onLoad = EU.runEffectFn3 _on "load"
```

It's very important to learn this to work with FFI with uncurried arguments, so it's worth visiting the links in this post to review this.

## Thunked Promises to Aff

This section is explained more in the docs for PureScript-Aff-Promise here: <https://pursuit.purescript.org/packages/purescript-aff-promise/2.0.0>

Because merely creating a Promise in JavaScript will run them, we need to actually thunk even the creation of Promises in FFI. This means that all of our FFI functions need to return `Effect (Promise Something)`.

Then we need to work with these Promises in our PureScript application. Almost all PureScript applications use the [PureScript-Aff](https://github.com/slamdata/purescript-aff) library to have reliable, fast asynchronous effects, so it's nicest to use the `Aff` type and convert to it. Luckily, we have just the function for it defined in Aff-Promise as [toAffE](https://pursuit.purescript.org/packages/purescript-aff-promise/2.0.0/docs/Control.Promise#v:toAffE):

```hs
toAffE :: forall a. Effect (Promise a) -> Aff a
-- from Control.Promise in Aff-Promise
```

So this lets us convert a thunked Promise into an Aff, so this lets us define FFI imports like this:

```hs
foreign import _newPage :: FU.Fn1 Browser (Effect (Promise Page))
```

and we can define a little helper function and define a normal function for newPage:

```hs
runPromiseAffE1 :: forall a o. FU.Fn1 a (Effect (Promise o)) -> a -> Aff o
runPromiseAffE1 f a = Promise.toAffE $ FU.runFn1 f a

newPage :: Browser -> Aff Page
newPage = runPromiseAffE1 _newPage
```

And there we have it, a normal function that uses `Aff` that we can call from PureScript without having to deal with its representation in the FFI.

## Conclusion

Hopefully this has shown you some of the ways you can work with FFI and convert FFI imports into normal PureScript functions and values and empowered you to solve problems as you see fit, rather than to be entrapped by some restrictive, partial solutions.

Be sure to visit the links to see more in-depth explanations.

## Extra

Readers also pointed out that I didn't talk about exposing foreign values as `Foreign` from the [PureScript-Foreign](https://github.com/purescript/purescript-foreign) library. This library along with [Simple-JSON](https://github.com/justinwoo/purescript-simple-json) can be quite useful for verifying that some FFI result is what you expect in runtime.

## Links

* PureScript language FFI documentation <https://github.com/purescript/documentation/blob/master/language/FFI.md>
* PureScript documentation guide on FFI <https://github.com/purescript/documentation/blob/master/guides/FFI.md>
* PureScript-Effect uncurried function documentation <https://pursuit.purescript.org/packages/purescript-effect/2.0.0/docs/Effect.Uncurried>
* PureScript-Aff-Promise documentation <https://pursuit.purescript.org/packages/purescript-aff-promise/2.0.0>