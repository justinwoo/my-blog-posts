Writing a simple Telegram chat bot in Purescript
...a follow-up to ["Async in Purescript is fun and easy"](http://qiita.com/kimagure/items/2ebce1399bac00c79656)

Last time I wrote an article about how async in Purescript is pretty fun and doesn't have to be hard, but that doesn't really give users who are unfamiliar with Purescript/Haskell/Elm a good sense of what writing a normal application looks like using those async tools. In this post, I'll go into a breadth of features that I used to make a simple Telegram chatbot and hopefully provide some examples of how to use `Eff`s and `Stream`s in the "context" of `Aff`.

## Getting to work

### Reading configuration

My chatbot some configuration in a JSON file with the following properties:

* `token` - auth token for my Telegram bot
* `torscraperPath` - path to my [Torscraper program](http://qiita.com/kimagure/items/5674e3ae9c87262af762)
* `master` - my Telegram user id, for when I need to send messages that aren't replies

So I need to read in this file, parse the JSON, then extract the fields (while making sure they are set). Since this only requires one value, I use an `Aff` here.

For reading the files, there is already a nice library, [purescript-node-fs-aff](https://github.com/purescript-node/purescript-node-fs-aff). It's nice that we don't need to do this manually. We use `readTextFile` which takes an encoding and a filepath.

```haskell
readTextFile UTF8 "./config.json"
```

For parsing the result, there's also another nice library, [purescript-foreign](https://github.com/purescript/purescript-foreign/). This lets us parse JSON and start reading props from it.

```haskell
parseConfig :: String -> Either ForeignError Config
parseConfig json = do
  value <- parseJSON json
  token <- readProp "token" value
  torscraperPath <- readProp "torscraperPath" value
  master <- readProp "master" value
  pure
    { token: token
    , torscraperPath: torscraperPath
    , master: master
    }
```

This seems meatier, but uses things we've seen before. In this `do` block, each binding is the result of taking the `Right` of our `Either` representing a successful operation. `Left` is a failure, and would short-circuit this to return the left side, which in this case is a `ForeignError`. After all these `readProp`s, we then lift our record of type `Config` into the `Either` as `Right Config`.

For putting these together, we can use the generic `map/<$>` operator on an `Aff` to map a function to what is inside, and we get what we're looking for.

```haskell
getConfig :: forall e. Aff (fs :: FS | e) (Either ForeignError Config)
getConfig = parseConfig <$> readTextFile UTF8 "./config.json"
```

The config parsing is a key point in our application's startup, so I'm going to make sure that the user is notified when they mess this up, which means that I'll be handling the `Left` case of the `Either` to notify the user their config file is malformed. The `Right` case will resume as needed.

```haskell
main = launchAff $ do
  config <- getConfig
  case config of
    Left e -> liftEff' $ log "config.json is malformed. closing."
    Right {token, torscraperPath, master} -> do
      [...]
```

You'll see here that I've used `liftEff'`, because I'm using `log` from `Control.Monad.Eff.Console`. This lifts the `Eff` into an `Aff` so that it can be used in the `do` block of the `launchAff`.

This will come up again and again, in this article and in other code you write.

### Starting up our bot and bot methods

The actual Telegram API is humongous, and this being just a hacky project, I have taken many liberties to make this as easy as possible for me.

For one, I made connecting dead easy:

```js
exports.connect = function (token) {
  return function () {
    return new TelegramBot(token, {polling: true});
  };
}
```

```haskell
type TelegramEffects e = (telegram :: TELEGRAM, console :: CONSOLE | e)

foreign import data TELEGRAM :: !

foreign import data Bot :: *

foreign import connect :: forall e. Token -> Eff (TelegramEffects e) Bot
```

I've made a convenience type to work from called `TelegramEffects`, but importantly I've added `TELEGRAM` as an effect and `Bot` as a data type for my bot instance. My `connect` function simply returns a bot instance running through the instance, and so usage looks like our logging function above:

```haskell
bot <- liftEff $ connect token
```

With sending messages, I also have a relatively simple FFI function:

```js
exports._sendMessage = function(bot, id, message) {
  return function () {
    bot.sendMessage(id, message);
  };
}
```

```haskell
foreign import _sendMessage :: forall e.
  Fn3
    Bot
    Int
    String
    (Eff (TelegramEffects e) Unit)
```

This `Fn3` thing may be unfamiliar to you. It's the uncurried function helper, so that you don't have to write fully curried FFI functions. `Fn3` is of kind `* -> * -> * -> * -> *`, which looks scary at first, but really means that it takes four arguments, which aligns to the three arguments and one return value. This has an accompanying `runFn3` function for actually running these uncurried functions.

My `sendMessage` then has some logic about when messages should actually be sent:

```haskell
data RequestOrigin
  = User
  | Timer

type Request =
  { origin :: RequestOrigin
  , id :: Id
  }

sendMessage :: forall e. Bot -> Result -> Eff (TelegramEffects e) Unit
sendMessage bot {id, output, origin} = do
  case origin of
    Timer ->
      case indexOf "nothing new to download" output of
        Just _ -> log "timer found nothing" -- don't care about being notified here!
        _ -> send
    _ -> send
  where
    send = do
      log output
      runFn3 _sendMessage bot id output
```

My message listening function is the worst, since I was too lazy to properly set up anything, including traversing the complex object structure to get just a few pieces of information I need. I "leave this" as an "exercise for the reader" if they would like to implement this correctly:

```js
exports.addMessagesListener = function (bot, User, eff) {
  return function () {
    bot.onText(/^get$/i, function (msg, match) {
      var fromId = msg.from.id;
      eff({
        origin: User,
        id: fromId
      })();
      console.log('got request from', fromId);
    });
  };
}
```

```haskell
foreign import addMessagesListener :: forall e.
  Fn3
    Bot
    RequestOrigin
    (Request -> Eff (TelegramEffects e) Unit)
    (Eff (TelegramEffects e) Unit)
```

### Running our Torscraper

This is another case where we can take advantage of existing code written in [purescript-node-child-process](https://github.com/purescript-node/purescript-node-child-process) and [purescript-node-streams](https://github.com/purescript-node/purescript-node-streams). This allows me to spawn a child process and add some handlers like usual. I collect the output using a ref that I write to.

```haskell
runTorscraper :: forall e.
  String ->
  Request ->
  Aff
    ( ref :: REF
    , cp :: CHILD_PROCESS
    | e
    )
    Result
runTorscraper path request = makeAff \e s -> do
  ref <- newRef ""
  process <- spawn "node" ["index.js"] $
    defaultSpawnOptions { cwd = Just path }
  result <- try $ onDataString (stdout process) UTF8 \string ->
    modifyRef ref $ append string
  case result of
    Right _ -> do
      onError process $ toStandardError >>> e
      onExit process \exit -> do
        output <- readRef ref
        s { id: request.id, origin: request.origin, output: output }
    Left err -> e err
```

### Putting it into `Streams`

Now that we have the pieces, we can start putting them into place.

The `addMessagesListener` from before can be used with `fromCallback` from purescript-xstream to create a `Stream`. This `fromCallback` actually returns a `Eff e (Stream a)`, so we need to lift this like before.

```haskell
requests <- liftEff $ fromCallback $ runFn3 addMessagesListener bot User
```

Similarly, we use `periodic` from purescript-xstream and then map a `const timerRequest` to it, so that every tick of the timer will just give us this static request.

```haskell
let timerRequest = {id: master, origin: Timer}
timer <- liftEff $ periodic (60 * 60 * 1000)
let timer' = const timerRequest <$> timer
```

Then we need to take these two `Stream`s, merge with another `Stream` that will make sure this gets called when the bot starts up, and switch-map an `Eff`. A switch-map is just like a normal flatMap, where you pass in a function of `a -> f a`, but in the context of `Stream`s/observables, the subscription to the previous provided `Stream` is disposed. This is a bit of a dense explanation, but there are better explanations and examples online like [this StackOverflow question](http://stackoverflow.com/questions/28175702/what-is-the-difference-between-flatmap-and-switchmap-in-rxjava).

So by using `switchMapEff` in purescript-xstream, we can switch-map `Eff`s to produce `Stream`s.

```haskell
results <- liftEff $ (requests <|> timer' <|> pure timerRequest) `switchMapEff` \request ->
  fromAff $ runTorscraper torscraperPath request
```

The `alt/<|>` operator is used here to merge all the values emitted by the streams.

Finally we can just lift the subscribe function and we're done:

```haskell
liftEff' $ addListener
  { next: sendMessage bot
  , error: message >>> log
  , complete: const $ pure unit
  }
  results
```

## Conclusion

Hopefully this has been kind of useful to see how you can write your own Purescript programs and how you can use `Eff`s and `Stream`s in the "context" of `Aff`.

If you made it this far, thanks for reading! And please do send me corrections or suggestions, as I'm sure I do not have the best solutions here.

## Links

* My Telegram bot code used in this post https://github.com/justinwoo/simple-rpc-telegram-bot/
* The previous post, "Async in Purescript is fun and easy" http://qiita.com/kimagure/items/2ebce1399bac00c79656
* The post about my Torscraper, "Making a simple node program with Purescript" http://qiita.com/kimagure/items/5674e3ae9c87262af762
* purescript-aff https://github.com/slamdata/purescript-aff/
* purescript-xstream https://github.com/justinwoo/purescript-xstream
* purescript-node-fs-aff https://github.com/purescript-node/purescript-node-fs-aff
* purescript-node-child-process https://github.com/purescript-node/purescript-node-child-process
* purescript-node-streams https://github.com/purescript-node/purescript-node-streams
