Record-based API Route-Handler pairing with Row Types
The other day, I was thinking of ways I could prevent a common mistake I make when registering routes for my vidtracker project: not registering a route handler. Even though I share the type definitions used for routes between my frontend and backend, it doesn't help if I forget to actually register a handler for the route.

Luckily, we have the technology to do this with the following:

1) Share the record label used between a record of my API Routes and Route Handlers. By using the same label, I know which is to be paired with which.
2) Use `RowToList` to convert my row types to type-level lists so I can iterate them. I also take advantage of the fact that the keys are sorted when converted to `RowList`s.
3) Use `RowCons` to constrain that the routes and handlers records both have an element for the given single labels.

## My Route Types

First, I have my own kind `RequestMethod`, for which I have two data types:

```hs
foreign import kind RequestMethod
foreign import data GetRequest :: RequestMethod
foreign import data PostRequest :: RequestMethod
```

This is then incorporated into my `Route` data type:

```hs
data Route (method :: RequestMethod) req res (url :: Symbol) = Route
```

As you can see, `Route` is a phantom type where I have an associated method, a request type, response type, and a `Symbol` for the URL. I then have two convenience type aliases:

```hs
type GetRoute = Route GetRequest Void
type PostRoute = Route PostRequest
```

The first alias prevents me from accidentally using the `req` parameter to try to read the response body, preventing me from writing invalid code that tries to read the request body of a GET request. This is just one example of how phantom types can be incredibly useful for preventing mistakes and save you from having to do extra manual checking work (if a computer can be told to do it fairly easily, why not, right?).

The second is a simple alias allowing me to use all of the remaining parameters.

Then I have my route definitions in a record like so:

```hs
apiRoutes ::
  { files :: GetRoute (Array Path) "/api/files"
  , watched :: GetRoute (Array WatchedData) "/api/watched"
  , getIcons :: PostRoute GetIconsRequest Success "/api/get-icons"
  , update :: PostRoute FileData (Array WatchedData) "/api/update"
  , open :: PostRoute OpenRequest Success "/api/open"
  , remove :: PostRoute RemoveRequest Success "/api/remove"
  }
apiRoutes =
  { files: Route
  , watched: Route
  , getIcons: Route
  , update: Route
  , open: Route
  , remove: Route
  }
```

So while the runtime representation isn't very interesting, the static information we get from the phantom type parameters is more than enough for us to statically prepare code paths.

## Iterating Routes and Handlers

So from (2) above, we can write our exposed function that works with any Monad:

```hs
registerRoutes :: forall routes handlers routesL handlersL m
   . RowToList routes routesL
  => RowToList handlers handlersL
  => Monad m
  => RoutesHandlers routesL handlersL routes handlers m
  => Record routes
  -> Record handlers
  -> m Unit
registerRoutes routes handlers =
  registerRoutesImpl
    (RLProxy :: RLProxy routesL)
    (RLProxy :: RLProxy handlersL)
    routes
    handlers
```

As usual, `routes` and `handlers` are the row type `# Type` from the routes and handlers records passed in, for which I create `RowList`s by using `RowToList`. Then I use the method for the implementation and pass in the records for my `RoutesHandlers` type class defined as such:

```hs
class RoutesHandlers
  (routesL :: RowList)
  (handlersL :: RowList)
  (routes :: # Type)
  (handlers :: # Type)
  m
  where
    registerRoutesImpl :: forall proxy
       . Monad m
      => proxy routesL
      -> proxy handlersL
      -> Record routes
      -> Record handlers
      -> m Unit
```

So our type class takes in the `RowList`s it can use to match instances and iterate with, the row types for the actual records, and our monad. *The biggest reason we explicitly pass in the row types is because otherwise, I would have to remove fields from my records to pass them down, whereas I only need to get fields out of them. If this note doesn't make sense, come back to it after you've read through this blog post and some other RowToList resources.*

So how do these get implemented?

### Nil/Nil case

So I want to make sure that when I go through the routes and handlers, I should reach the `Nil` case on both at the same time. By defining a `Nil Nil` case and not `Nil xs` or `xs Nil`, I am able to enforce this (otherwise there is no matching instance for this case). And so in the case of `Nil Nil`, I need to do nothing, so the instance is defined as such:

```hs
instance routesHandlersNil :: RoutesHandlers Nil Nil trash1 trash2 m where
  registerRoutesImpl _ _ _ _ = pure unit
```

### Cons name/Cons name case

So since the `RowList` is sorted by the label, we have guaranteed pairwise iteration, which is quite convenient for (1), that we use the same label. By writing our instance head to use the same variable `name`, we can make sure that the instance matches when the `name`s are the same:

```hs
instance routesHandlersCons ::
  ( RoutesHandlers rTail hTail routes handlers m
  , IsSymbol name
  , RowCons name handler trash1 handlers
  , RowCons name route trash2 routes
  , RegisterHandler route handler m
  ) => RoutesHandlers
         (Cons name route rTail)
         (Cons name handler hTail)
         routes
         handlers
         m where
  registerRoutesImpl _ _ routes handlers = do
    registerHandlerImpl (get nameP routes) (get nameP handlers)
    registerRoutesImpl (RLProxy :: RLProxy rTail) (RLProxy :: RLProxy hTail) routes handlers
    where
      nameP = SProxy :: SProxy name
```

From top to bottom:

* The tails of the routes and handlers `RowList`s, along with the original record row types and the Monad `m` are passed down to run through the rest of the list.
* The `name` needs to be a `Symbol` for some of our operations below.
* We constrain with `RowCons` that there is a field with a label `name` and type `handler`, added on to a `trash1` row that we don't really care about, which forms the `handlers` row. Put another way, we ensure that `name :: handler` exists in our `handlers` row type.
* The same is done with `route` and `routes`.
* Then we use our other type class `RegisterHandler`, which provides `registerHandlerImpl`.

The body here does two things. First, it uses `RegisterHandler` to register the `route` and the `handler` that we're getting out of the records. Then, it uses `RoutesHandlers` to operate on the rest of the routes and handlers. Now to look more at `RegisterHandler`.

## Registering our Handlers

For our type class here, we need a single route, its associated handler, and the monad that we're operating with.

```hs
class RegisterHandler route handler m
  | route -> handler m
  where
    registerHandlerImpl :: route -> handler -> m Unit
```

I use the functional dependencies here to make sure that the correct instance is chosen based on the route. I do this because I need to handle the two cases I had above: the POST and GET cases.

As I wrote earlier, the handlers correspond to the route definition:

```
route: Route PostRequest FileData      (Array WatchedData) "/api/update"
handler:                 FileData -> m (Array WatchedData)

route: Route GetRequest Void (Array WatchedData) "/api/watched"
handler:                   m (Array WatchedData)
```

So with this information, we can write the instances accordingly.

### POST

For ease of use and implementation, I provide my handlers as functions for `Aff`, and use the `purescript-express` `AppM` for the monad returned.

```hs
instance registerHandlerPost ::
  ( IsSymbol url
  , ReadForeign req
  , WriteForeign res
  ) => RegisterHandler
         (Route PostRequest req res url)
         (req -> Aff (express :: EXPRESS | e) res)
         (AppM (express :: EXPRESS | e)) where
  registerHandlerImpl route handler =
    E.post route' handler'
    where
      route' = reflectSymbol (SProxy :: SProxy url)
      handler' = do
        body <- getBody
        case runExcept (read body) of
          Right (r :: req) -> do
            response <- liftAff $ handler r
            sendJson $ write response
          Left e -> do
            setStatus 400
            sendJson $ write {error: show e}
```

So the constraints are the `IsSymbol url` again to reflect the URL to use to register the route, `ReadForeign req` for parsing the request JS object to the type I want to work with, and `WriteForeign res` for encoding the response type to the JS object expected.

Note that in the instance head, I am doing the matching that I wrote about again: the `req` and `res` types are in the corresponding positions such that the handler must be correctly typed and implemented for the route. This lets me avoid any problems with accidentally returning the wrong type from a given route.

The implementation, on the other hand, is all normal code you'd write for `purescript-express`.

### GET

With the POST instance written, the GET instance comes out about the same:

```hs
instance registerHandlerGet ::
  ( IsSymbol url
  , WriteForeign res
  ) => RegisterHandler
         (Route GetRequest Void res url)
         (Aff (express :: EXPRESS | e) res)
         (AppM (express :: EXPRESS | e)) where
  registerHandlerImpl route handler =
    E.get route' handler'
    where
      route' = reflectSymbol (SProxy :: SProxy url)
      handler' = do
        response <- liftAff handler
        sendJson $ write response
```

In this case, as `req` does not exist for GET requests, the handler provided doesn't need to be anything more than `Aff res`.

## Usage

And that's about it, really. The actual usage then looks like this:

```hs
routes config = do
  -- ...
  registerRoutes
    apiRoutes
    { files: getFiles config
    , watched: getWatchedData config
    , getIcons: getIconsData config
    , update: updateWatched config
    , open: openFile config
    , remove: removeFile config
    }
  -- ...
```

And while we *could* fix this up in a variety of ways such as using `ReaderT` with this `config` for `Aff` or writing another fancy function to apply `config` to a heterogenous functions of `config`, really, we don't need to for now, since this is not really error-prone as you can't forget to apply `config`.

Now all the `apiRoutes` can be called from my client and the handlers are guaranteed to be implemented here.

## Conclusion

People have been asking me for a while about how they might go about using row types and these strange `RowCons` and `RowToList` tools and such, but I hope this provides more examples of how some of this stuff works.

If you have any questions about this or anything else PureScript, please ask on the [FP Slack](https://fpchat-invite.herokuapp.com/) #purescript or #purescript-beginners channel, on [r/purescript](https://www.reddit.com/r/purescript/), or me directly on [Twitter@jusrin00](https://twitter.com/jusrin00)!

## Links

* https://github.com/justinwoo/vidtracker/
* some extra record functions with various implementations and examples https://github.com/justinwoo/purescript-record-extra
* thread about RowToList https://www.reddit.com/r/purescript/comments/6mss5o/new_in_purescript_0116_rowtolist/
