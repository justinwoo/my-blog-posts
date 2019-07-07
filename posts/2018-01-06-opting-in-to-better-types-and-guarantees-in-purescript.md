# Opting in to better types and guarantees in PureScript

For a while now I've had this [vidtracker](https://github.com/justinwoo/vidtracker) which helps me keep track of shows I'm watching, and acts as a simple playground for ideas I have about writing simple web apps in PureScript.

Specifically, I think that the definition of routes is somewhere where you can get a lot of guarantees, but you opt-in to how strong of guarantees you want. In this post, I'll go into the various stages of [this meme](https://twitter.com/jusrin00/status/943439105144295424) (minus the last joke entry) and what they meant for my project and what kind of bugs that I originally introduced myself that they fixed.

## Background

Originally, I had planned to have this be a small app that I would not do much with, so I wrote it in Elixir just to try it out. After working with Elixir turned out to not be very good for various reasons, I rewrote this in PureScript and eventually replaced the Cycle.js frontend with Halogen. After roughly 10 months, I now compile four whole programs out of a single codebase, with PureScript's built-in dead code elimination making it possible to automatically only bundle specific exports from modules as needed. The four parts are:

1. A backend that runs on Node, using express bindings
2. A frontend written with Halogen, with a component that uses echarts for a heatmap
3. A static CSS generator, using PureScript-CSS
4. A web-page scraper for icons

*One thing that maybe many don't know is that PureScript has had type-safe definition and rendering of both inline and static CSS since April 2015 as authored by Brian McKenna. While I could talk about this topic more, essentially it boils down to using a [very simple program](https://github.com/justinwoo/vidtracker/blob/fe531cd2207d24545275ebea473f2d7a776ecd38/src/GenerateStylesheet.purs#L16) to render a [type-checked stylesheet](https://github.com/justinwoo/vidtracker/blob/fe531cd2207d24545275ebea473f2d7a776ecd38/src/FrontEnd/Style.purs#L64) to a file.*

## Simple record of String URL and String Method

```hs
type Route =
  { url :: String
  , method :: String
  }
```

This is probably one of the most common ways to encode some collection of routes. Assuming that you use the same record value definitions between your front and back end, you at least get the guarantee that for a given record, the url should have been registered in the backend with the correct HTTP method.

Unfortunately, this lacks in some important ways in that there are two main obvious missing parts: the type of the request and the type of the response. With the request not being in here, there's no telling at compile time if the serialized value that you may or may not have put in the request will work. Similarly, since the response body is not known, you need to choose what type to deserialize to and hope that you didn't accidentally create a code path that will always fail.

Putting the two deficiencies together, I often wrote very obvious bugs like so:

```hs
getResult :: _ (VE (Tuple (Array Path) (Array WatchedData)))
getResult = do
  files <- getJSON "/api/files" -- good
  watched  <- getJSON "/api/files" -- lol borked code
  pure $ Tuple <$> files <*> watched
```

Which, sure, you could then replace it with the record Routes, but then I also had these problems:

```hs
getResult :: _ (VE (Tuple
                     (Array Path) -- good
                     (Array Path) -- lol borked code
                     ))
getResult = do
  files <- getJSON files.url
  watched  <- getJSON watched.url
  pure $ Tuple <$> files <*> watched
```

## Phantom type

```hs
newtype Route request response = Route
  { url :: String
  , method :: String
  }
```

This is an approach I originally wrote about [here](https://qiita.com/kimagure/items/b576b5bfe370180599f8) where I go into how I made vidtracker a full stack PureScript app. By using a phantom type to hold the parameters for my request and response types, I am able to correctly associate a route definition with a request and response type. Of course, just defining this type alone isn't enough, as a normal ajax call from the front end is roughly of type `{ url :: String, method :: String, content :: Maybe String } -> Aff effects String`. There is nothing wrong with this original ajax function, but it doesn't quite meet my needs when wanting to have type safe Routes.

To be able to work with a safe request function, I defined a function that wrapped this with the appropriate JSON serialization and deserialization constraints and methods:

```hs
postRequest :: forall req res m eff.
  MonadAff -- ensures m can handle Aff
    ( ajax :: AJAX | eff )
    m
  => WriteForeign req -- ensures that the request body can be serialized to JSON
  => ReadForeign res -- ensures that the response body can be deserialized from JSON

  => Route req res -- my route definition from above,
                      where I get access to the req/res parameters
  -> req -- the response type before serialization
  -> m (VE res) -- a response with an Either of the Validation result of parsing
                   the response body JSON to res
```

And now this lets me correctly make requests to a given URL with the correct response body, and handle the expected response body. This makes sure that I only have N number of route-request-response combinations, whereas having them be independent would leave me with a N * M * L combination problem. As many like to say -- make invalid states impossible :)

On the back end side, I can use this same phantom type information to register a handler that only handles the correct corresponding request-response pairs.

```hs
registerHandler :: forall req res
   . ReadForeign req
  => WriteForeign res
  => Route req res
  -> handler
  -> m Unit
```

Even with just this, there are very strong guarantees provided here, but there are still two things missing: our URL is still a string, while we know statically what it should be, and our method is also a string, even though we know statically what it should be. By having these as value-level properties, we leave them susceptible to modification and becoming out-of-sync with the correct values.

## Phantom type with non-Type kinds to guarantee static values

```hs
data Route
  (method :: RequestMethod)
  request
  response
  (url :: Symbol)
  = Route
```

In reality, there's not that much of a stretch between this and the previous section, except for the addition of non-Type parameters method and url. The RequestMethod kind is defined as such:

```hs
foreign import kind RequestMethod
foreign import data GetRequest :: RequestMethod
foreign import data PostRequest :: RequestMethod
```

Such that this kind does not have a value representation (as that would be of kind Type), but can be used with a type class to obtain the HTTP Method values that correspond at each call site that requires these.

A [Symbol](https://pursuit.purescript.org/builtins/docs/Prim#k:Symbol) is a type-level string that does not change in its "type-level value". This gives us the ability to reflect this into a value-level string for implementations, but still guarantee that we work with this exact string otherwise.

These two allow me to tweak my front end POST request function:

```hs
post :: forall method req res url m eff.
  MonadAff
    ( ajax :: AJAX | eff )
    m
  => WriteForeign req
  => ReadForeign res
  => IsSymbol url
  => PostRoute req res url -> req -> m (VE res)
```

So the `IsSymbol` constraint gives me access to `reflectSymbol` function to convert this into a string.

You might notice that I have a PostRoute here. It is defined like so:

```hs
type GetRoute = Route GetRequest Void
type PostRoute = Route PostRequest
```

This means that I can't use the same function for PostRoute as with GetRoute, as PostRoute takes three more parameters while GetRoute takes only two (as there is no associated request body for a GET request in this model). While this seems like an annoyance, this also gives me greater correctness by using the Void type here -- there is no value for Void, and it can't be serialized to JSON, so this prevents mistakes caused by trying to treat GET routes in the same way.

### Bonus: route-handler pairwise handling

Of course, this can be solved by using a type class defined for Route, which I did in my post [here](https://qiita.com/kimagure/items/bb9bd3e4ffe1bba4c214). In this post, instead of putting my route definitions separately, I bundled them all together in a record. By using a record, I then was able to write a type class which used the row types converted to RowLists to iterate, and I ensured that every route handler in this routes record was defined and registered. This is just one more way I opted-in to stronger guarantees, since I commonly made the mistake of forgetting to register routes.

*I also wrote about this topic in Haskell [here](https://qiita.com/kimagure/items/7c3521cfbf00ad173801).*

## Conclusion

Hopefully this post was able to show that when you use a statically typed language, ultimately it is up to you to choose how many guarantees you actually want to get out. While libraries may expose you some barebones functions that are more easily composed with other libraries you get, ultimately you may end up with a solution like this that doesn't involve too much buy-in to details you might not care about for the moment, and gives you great control over the parts where you do care. *Unfortunately, how strong of guarantees you can get are limited by your language's features and culture.*

So there you have it, 1500+ words to explain a functional programming meme.

## Links

* vidtracker repo: https://github.com/justinwoo/vidtracker
* vidtracker with simple phantom types: https://qiita.com/kimagure/items/b576b5bfe370180599f8
* row types to guarantee route-handler pairs: https://qiita.com/kimagure/items/bb9bd3e4ffe1bba4c214