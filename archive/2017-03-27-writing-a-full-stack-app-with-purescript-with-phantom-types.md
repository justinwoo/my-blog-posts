Writing a full-stack app with Purescript with phantom types
Over the last couple of years, I've had a basic problem one would solve with a CRUD app -- I want to be able to easily open videos from a directory listing and then mark specific videos as watched. I first made a basic implementation for myself using Javascript and Elixir (mostly to experiment with Elixir), but found myself wanting everything that's good about Purescript that understandably doesn't exist in Elixir, so I decided to make the front-end and backend both in Purescript.

# Tl;dr

## Routes

I defined a "Routes" module that contained the following data type:

```hs
newtype Route req res = Route
  { method :: Method
  , url :: String
  }
```

With this, I would use the phantom types for Request and Response bodies to specify the types for generic functions I would later use.

For an unused request or response, I created a newtype for string that would simply swallow any inputs, like so:

```hs
newtype Unused = Unused String
derive instance ntU :: Newtype Unused _
derive newtype instance isUU :: IsForeign Unused
derive newtype instance asUU :: AsForeign Unused
```

And then for my actual routes, I simply needed four features:

1. a list of the files in my videos directory
2. a list of watched videos and their statuses
3. a way to open the videos
4. a way to update a video's info and return (2)

and defining these would be quite straightforward:

```hs
files :: Route Unused (Array Path)
files = Route {method: GET, url: "/api/files"}

watched :: Route Unused (Array WatchedData)
watched = Route {method: GET, url: "/api/watched"}

open :: Route OpenRequest Success
open = Route {method: POST, url: "/api/open"}

update :: Route FileData (Array WatchedData)
update = Route {method: POST, url: "/api/update"}
```

## Types

I defined a "Types" module which had the definition of all these request and response body types and their instances for `IsForeign` and `AsForeign` for de/serializing JSON. And so, all of these had similar definitions like so:

```hs

newtype FileData = FileData
  { path :: Path
  , watched :: Boolean
  }
derive instance eqFD :: Eq FileData
derive instance ordFD :: Ord FileData
derive instance grFD :: Generic FileData _
instance ifFD :: IsForeign FileData where
  read = readGeneric'
instance afFD :: AsForeign FileData where
  write = toForeignGeneric'
```

By using the route definitions with phantom types for the request and response body types, I was able to not worry about mismatching the URLs and types (as I usually do) not only on the front end, but also on the back end.

That's pretty much the whole show! The rest is about my Halogen front-end and Hyper backend.

# The Halogen Front-End

If you'd like a more in-depth tutorial on Halogen, please see [this post](http://qiita.com/kimagure/items/653c52e77d7cd3567498) and/or [The Halogen guide](https://github.com/slamdata/purescript-halogen/blob/master/docs/README.md) first!

## Types

Unsurprisingly, there isn't much state that needs to be kept here:

```hs
type State =
  { files :: Array Path
  , watched :: Array WatchedData
  }
```

Likewise, the queries we will are quite simple:

```hs
data Query a
  = Init a
  | OpenFile Path a
  | SetWatched Path Boolean a
```

`Init` will run at the beginning of our component's lifecycle, `OpenFile` will send a request to open a video, and `SetWatched` will send a request to set/unset a video as watched.

```hs
type AppEffects eff =
  Aff
  ( ajax :: AJAX
  , console :: CONSOLE
  | eff )
```

These are the kinds of effects used by my component. Seems normal enough, right?

## Component

The type of my component comes out at such:

```hs
ui :: forall eff. H.Component HH.HTML Query Unit Void (AppEffects eff)
```

and its definition so:

```hs
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
```

We use a lifecycle component here since we need to do some initialization to load our files and watched data. We use the initializer property to define what should be run.

### State

Unsurprisingly, the initial state we give our state's array properties can just be so:

```hs
initialState :: State
initialState =
  { files: mempty
  , watched: mempty
  }
```

The `mempty`s just resolve to an Array's Monoid identity `[]`. This is handy for me to both forget about the concrete implementation in favor of the generic definition, but also to allow me to eventually replace this with a Map or Set type without the need to refactor concrete identities, hooray typeclasses!

### Render

Not a whole lot involved here either, but there are some parts where I register callbacks for click events to fire off various queries.

```hs
render :: State -> H.ComponentHTML Query
render state =
  HH.div
    [ HP.class_ $ wrap "container" ]
    $ HH.h1_ [ HH.text "Vidtracker" ]
      : (file <$> state.files)
  where
    file path =
      HH.div
        [ HP.class_ $ wrap "file"]
        [ HH.a
          [ HP.class_ $ wrap "file-link"
          , HE.onClick $ HE.input_ (OpenFile path) ]
          [ HH.text $ unwrap path ]
        , HH.button
          [ HP.classes $ wrap <$>
            [ "file-button"
            , "pure-button"
            , maybe "" (const "pure-button-primary") watched
            ]
          , HE.onClick $ HE.input_ (SetWatched path (not $ isJust watched))
          ]
          [ HH.text $ maybe "not watched" (const "watched") watched ]
        , HH.span
          [ HP.class_ $ wrap "file-note" ]
          [ HH.text $ maybe "" id watched ]
        ]
      where
        watched = getDate <$> find (\(WatchedData fd) -> fd.path == path) state.watched
        getDate (WatchedData {created}) = JSDate.toDateString <<< unsafePerformEff <<< JSDate.parse $ created
```

The reason why `unsafePerformEff` is here is because `toDateString` depends on the locale of your browser, but personally, I'm fine with displaying random dates so I preferred to do this here. Otherwise, we could also parse our dates in our eval function and prepare them beforehand.

### Eval

My eval function comes out to the following type and functions:

```hs
eval :: Query ~> H.ComponentDSL State Query Void (AppEffects eff)
eval (Init next) = do
  getResult >>= unV'
    \(Tuple f w) -> H.modify _ {files = f, watched = w}
  pure next
  where
    getResult = do
      files <- request files Nothing
      watched <- request watched Nothing
      pure $ Tuple <$> files <*> watched

eval (OpenFile path next) = do
  request open $ Just (OpenRequest {path})
  pure next

eval (SetWatched path flag next) = do
  request update (Just $ FileData {path, watched: flag}) >>= unV'
    \w -> H.modify _ {watched = w}
  pure next
```

It all seems pretty normal, but of course, there's no explicit type definitions here and everything works! The secret lies in the `request` function.

## Cooking with Types

I'm using Data.Validation.Semigroup here to correctly append my `NonEmptyList ForeignError` to each other, since sometimes I have multiple requests that need to go out and validate. And so I have the type alias:

```hs
type VE a = V (NonEmptyList ForeignError) a
```

And so my request function is defined:

```hs
request :: forall req res m eff.
  MonadAff
    ( ajax :: AJAX
    | eff
    )
    m
  => AsForeign req
  => IsForeign res
  => Route req res -> Maybe req -> m (VE res)
request (Route route) body =
  H.liftAff $ either invalid pure <$> parseResponse <$> action
  where
    action = AJ.affjax $ AJ.defaultRequest
      { method = Left route.method
      , url = route.url
      , content = unsafeStringify <<< write <$> body
      }
    parseResponse response =
      runExcept $ readJSON response.response
```

Looking at the signature, we can see that we only define type variables for our request, response, the Monad in which our Aff will run, and effects. We define the constraints for these as usual, especially with `AsForeign req`, meaning that our request body is representable as a JSON string, and `IsForeign res`, meaning that our response body is parseable from JSON.

Our function takes the Route value which has its req and res types defined within it, meaning that we will be using those types for our constraints when working with this. We take an optional request body and then return in our monad the validation result of our response body.

The definition itself is basically just plumbing, but you can see that in the term level, there exists only our route's method and url properties, but the phantom types for response and request are used to specify to our generic JSON functions to what types our JSON should be written and read from. (Isn't this freaking amazing???)

# Backend

For my backend, I'm using [Hyper](https://github.com/owickstrom/hyper). The way I'm using it isn't the most pretty, so I'll leave a lot of the plumbing out of this post. The most important bit is how I'm handling connections:

```hs
handleConn conn@{components: Config {dir, db, openExe}} =
  case Tuple conn.request.method conn.request.url of
    t
      | match t files -> handleFiles files
      | match t watched -> handleWatched watched
      | match t open -> handleOpen open
      | match t update -> handleUpdate update
      | otherwise -> fileServer "dist" notFound
    where
      bind = ibind
```

The connection is a record which has a components property for storing things I initialize and shove in my server: `dir` being the directory that my files live, `db` being my SQLite connection, and `openExe` being the target executable, explorer.exe or open (Thanks to Matti Hänninen for the last one and some other enhancements!).

I then use guards to handle the tuple of my method and url to match them to routes, defined as such:

```hs
match :: forall req res. Tuple (Either Method CustomMethod) String -> Route req res -> Boolean
match (Tuple m u) (Route {method, url}) =
  case m of
    Left m' -> m' == method && u == url
    _ -> false
```

As in my front end, I pass through my route value to take advantage of its types in my definitions, like so:

```hs

respondJSON json =
  writeStatus statusOK
  :*> headers [Tuple "Content-Type" "application/json"]
  :*> respond json
respondJSON' :: forall req res. (AsForeign res) => Route req res -> res -> _
respondJSON' _ = respondJSON <<< unsafeStringify <<< write

handleFiles r = do
  files <- lift' $ readdir' dir
  respondJSON' r files

handleFiles r = do
  files <- lift' $ readdir' dir
  respondJSON' r files

handleWatched r = do
  rows <- queryDB' "SELECT path, created FROM watched;" []
  respondJSON $ unsafeStringify rows

handleOpen r = withBody r \(OpenRequest or) -> do
  _ <- liftEff $ spawn openExe (pure $ concat [dir, unwrap or.path]) defaultSpawnOptions
  respondJSON' r $ Success {status: "success"}

handleUpdate r = withBody r \(FileData ur) -> do
  _ <- if ur.watched
    then queryDB' "INSERT OR REPLACE INTO watched (path, created) VALUES ($1, datetime());" [unwrap ur.path]
    else queryDB' "DELETE FROM watched WHERE path = $1" [unwrap ur.path]
  handleWatched watched

queryDB' query params = lift' $ queryDB db query params
```

And so, using the route term, I'm able to use the phantom type to type check the response that will be serialized to JSON. There are probably many better ways to do this, probably starting at the route definition level, but this is what I have for now.

## Conclusion

So hopefully, I've shown that phantom types can be very useful for when you want to work with concrete values that then provide types for "context" such that you can write functions that type check according to them, and that they are especially useful on the front-end, but also quite useful for making sure your backend is typed with those same definitions.

You can see a video demo [in this tweet](https://twitter.com/jusrin00/status/843025971234177024).

Let me know what you think of writing full-stack Purescript apps in this style [on Twitter](https://twitter.com/jusrin00)!

## Links

* Repo: https://github.com/justinwoo/vidtracker
* How to Foreign-Generic voodoo: https://github.com/justinwoo/purescript-howto-foreign-generic
* Halogen: https://github.com/slamdata/purescript-halogen
* Hyper: https://github.com/owickstrom/hyper
