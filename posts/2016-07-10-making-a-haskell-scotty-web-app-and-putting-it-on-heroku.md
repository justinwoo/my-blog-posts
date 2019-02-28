This won't be a very in-depth tutorial or anything, but should serve to provide an updated reference for how to make a simple Haskell web app using Scotty that connects to Redis and serves out some static assets (in this case, my Elm app).

This project uses [stack](haskellstack.org) and requires little more than `haskell-stack` on homebrew and npm.

## Config

### Haskell

Once the stack project init has run, you'll have to add some dependencies to `[your-proj].cabal`:

```
build-depends:       base
                   , scotty <- scotty and some deps
                   , clay
                   , text
                   , blaze-html <- blaze for html
                   , blaze-markup
                   , wai-extra <- stuff for the web app interface
                   , wai-middleware-static
                   , hedis <- redis lib for haskell
                   , directory <- system directory info stuff
                   , regex-base <- regex packages
                   , regex-tdfa
                   , regex-compat-tdfa
                   , bytestring <- because many libs use bytestrings
                   , transformers <- monad transformers, e.g. liftIO
                   , aeson <- aeson for JSON
```

In addition, I've found I've had to declare my other modules like so: (please correct me if I've messed it up)

```
other-modules:       Database
                   , Routes
                   , Views.Index
                   , Views.Layout
```

### Web

I also have a front-end project within in a directory called "web". Probably the config can live in your main project folder, I just made it this way out of laziness.

```
web/
  src/
    Main.elm
    index.js
  package.json
  webpack.config.js
```

```
in project.json:
  "devDependencies": {
    "elm-webpack-loader": "^3.0.3",
    "webpack": "^1.13.1"
  },

webpack.config.js:
  module.exports = {
    entry: [
      './src/index.js'
    ],
    output: {
      filename: 'dist/index.js'
    },
    module: {
      loaders: [{
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack'
      }]
    }
  };
```

We'll come back to Heroku set up at the very end.

## Code

### Haskell

I don't necessarily want to go into everything, but I'll try to have the important bits included.

Main:

```haskell
import Database (connectInfo, getDBConnection, seedDB)
import Routes (routes)

main :: IO ()
main = do
  -- get the list of my show files, extract the show name, and then remove duplicates
  myShows <- nub . getNames <$> getShows

  -- read in environment vars
  port <- read <$> getEnv "PORT"
  host <- getEnv "REDIS_HOST"
  redisPort <- getEnv "REDIS_PORT"
  auth <- getEnv "REDIS_AUTH"

  -- get a connection to my DB using my own connectInfo constructor helper
  -- also make sure my auth key has been packed into a bytestring
  conn <- getDBConnection $ connectInfo host redisPort $ pack auth

  -- run the seeding of my DB
  _ <- seedDB conn myShows

  -- start up scotty now
  scotty port $ do
    -- apply static asset delivery using the provided path
    middleware $ staticPolicy $ addBase "web/dist"

    -- apply my routes function
    routes conn myShows
```

Routes:

```haskell
routes :: Connection -> [String] -> ScottyM ()
routes conn myShows = do
  get "/" $ do
    -- lift the IO operation from the standard ScottyM action
    -- where I'm encoding data I get and showing it as a String
    -- mapped to the result of fetching my shows data
    myShows' <- liftIO $ show . encode <$> fetchShowsData conn myShows

    -- build html with blaze using that string input
    indexView myShows'

  post "/increment" $ do
    -- get "name" from the form data params
    name <- param "name"

    -- do an incrementShow operation
    result <- liftIO $ incrementShow conn name

    -- handle the result
    case result of
      -- when it works, we can build html packing the string showing of the result
      Right int -> html $ pack $ show int

      -- very lazy error case handling
      _ -> html "u suck"
```

Database:

```haskell
-- declare the MyShow type and its fields
data MyShow = MyShow
  { name :: String
  , count :: String
  }

-- then instance Data.Aeson.ToJSON accordingly for how to serialize MyShow
-- (this can be generically derived though, I did it for verbosity)
instance ToJSON MyShow where
  toJSON (MyShow name' count') = object ["name" .= name', "count" .= count']

-- fetchShowsData looks like this: (it's not actually *as* shitty as it looks at first)
fetchShowsData :: Connection -> [String] -> IO [MyShow]
fetchShowsData conn xs =
  -- map my extract method to the inner functor (my list of shows)
  -- and map that to the result of getting each show from the list xs
  -- traverse lets me do all the IO operations and return the result
  (<$>) extract <$> getShow `traverse` xs

  where
    -- run the redis command using the connection
    -- and map the result with a tuple of (x, result)
    -- using the command GET with my key prefixed appropriately
    getShow x = runRedis conn $ (x,) <$> get (prefix x)

    -- extract the tuple information to a MyShow
    extract (x, y) =
      MyShow
        { name = x
        , count = case y of
            Right (Just a) -> unpack a
            _ -> "N/A"
        }

-- updateShow (and inc, dec) are much simpler:
data UpdateShow = INCREMENT | DECREMENT

updateShow :: UpdateShow -> Connection -> String -> IO (Either Reply Integer)
updateShow update conn =
  -- run the command specified and prefix the provided key accordingly
  runRedis conn . command . prefix

  where
    -- the ADT argument breaks down to two operations we support here
    command = case update of
      INCREMENT -> incr
      DECREMENT -> decr

-- create an incrementing updateShow function
incrementShow = updateShow INCREMENT

-- vice versa
decrementShow = updateShow DECREMENT
```

Views.Index:

```haskell
injectScript :: String -> Html
injectScript = script . toHtml

-- it's HTML. not too much involved.
indexView showData = html . renderHtml $ layout "tracker" $ do
  div ! class_ "container-fluid" $
    div ! id "app" $ mempty
  injectScript $ printf "showData = JSON.parse(%s);" showData
  script ! src "index.js" $ mempty
```

And that's about it, really, for my Haskell code.

### Elm

It's a pretty normal Elm application now that you don't have a lot of flexibility in 0.17. I do take the show updates and shove them in ports though, as the normal HTTP solutions provided in other user libs don't have enough for what I need.

```haskell
main : Program { showData : List Show }
main =
  programWithFlags
    { init = \x -> ( { initModel | showData = x.showData }, Cmd.none )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

port showDataUpdates : (Show -> msg) -> Sub msg

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ downs handleKeyDown
    , ups handleKeyUp
    , showDataUpdates ShowDataUpdate
    ]

port updateRequests : { target : String, name : String } -> Cmd msg

updateShow : String -> Model -> Cmd msg
updateShow target { showData, cursor } =
  let name =
    case showData !! cursor of
      Just x -> x.name
      Nothing -> ""
  in
    updateRequests
      { target = target
      , name = name
      }

incrementShow = updateShow "increment"
decrementShow = updateShow "decrement"
```

And so for the JS code that uses this:

```js
var Elm = require('./Main.elm');

var app = Elm.Main.embed(document.getElementById('app'), {
  showData: window.showData
});

app.ports.updateRequests.subscribe(function (x) {
  console.log(x)
  const xhr = new XMLHttpRequest();
  xhr.open('POST', x.target, true);
  xhr.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
  xhr.send('name=' + x.name);
  xhr.onload = function (e) {
    app.ports.showDataUpdates.send({
      name: x.name,
      count: e.target.response
    });
  };
});
```

And that's it for my front-end code.

## Deployment on Heroku

There's a few things we need to deploy to Heroku.

### Environment

I went through and made a Heroku app and used the Heroku Redis add-on. I then linked my Github project, set the buildpack to `https://github.com/mfine/heroku-buildpack-stack`, and set my environment variables needed for this project (`REDIS_HOST`, etc).

### Code

You'll have to make a Procfile in your repo, so I did so with `web: .local/bin/tracker-hs`. Note that you'll have to match your executable target's name in your cabal file and make sure the path to the binary is correct. Otherwise, you can start digging with `heroku run bash`.

After all this, you should be able to deploy a branch and have it all work. Note that for my front-end, I'm choosing to build the front-end and push a `prepared-dist` branch, since I'm too lazy to do that correctly (you might use Travis to do that kind of thing, maybe).

## Links

* My Project: https://github.com/justinwoo/tracker-hs
* Stack - haskellstack.org
* Scotty - https://hackage.haskell.org/package/scotty
* Hedis - https://hackage.haskell.org/package/hedis
* Heroku Stack Buildpack - https://github.com/mfine/heroku-buildpack-stack