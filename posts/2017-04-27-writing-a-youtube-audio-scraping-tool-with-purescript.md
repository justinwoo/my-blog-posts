Previously I wrote about this topic [here](http://qiita.com/kimagure/items/5674e3ae9c87262af762), but that was a while ago and before I had contributed back some useful libraries. I recently did a refactor that removed all of the FFI code in the project so that it was only 153 lines (28 lines being imports) of Purescript!

## Why

There are Youtube channels out there with basically all audio content (music, "podcasts", etc.) that I wanted to rip using `youtube-dl` so that I could listen to them whenever I'm on a flight. I had a couple conditions:

* Manually opening each channel is too much work, no way I'm doing that
* I don't want to try to download stuff I already have
* I want to delete old files, so only checking for existing files is no good
    * Seriously, sometimes these audio files were like 200MB
* I want to manage the output files manually

Well, with unreasonable requirements like these, of course I had to write my own solution!

## How

There are roughly three stages of my program:

Main:

1. Read config
    * Exit if config is malformed
1. Connect to my database
1. For each channel specified in the config, download the links

Downloading links from a channel:

1. Get the HTML of the page
1. Try to parse the HTML and extract out links
    * Exit is the parse fails
1. For each link, downloading the audio

Downloading a link:

1. Check if I've already downloaded this link before
    * If so, just skip to the next
2. Run `youtube-dl` on the link to download and extract audio
3. Put an entry in our database that I've downloaded this link now

### Main

I run my program using [purescript-aff](https://github.com/slamdata/purescript-aff/) to perform asynchronous effects as I need:

```hs
-- type alias for all the effect types in my program
type Program e =
  ( ajax :: AJAX
  , console :: CONSOLE
  , cp :: CHILD_PROCESS
  , fs :: FS
  , db :: DBEffects
  | e
  )

main :: forall e.
  Eff
    (Program (exception :: EXCEPTION | e))
    (Canceler (Program e))
main = launchAff do
  decoded <- decodeJSON <$> readTextFile UTF8 "./config.
```

`readTextFile` is of the `Aff` type, allowing me to bind the inner value to a name in my do block. I map my function to decode the JSON contents of config.json.

My config file has only one property I'm concerned about: targets. This is an array of youtube channels that I will go through. Even though I could write this decoder manually, I chose to use [foreign-generic](https://github.com/paf31/purescript-foreign-generic/) to automatically define a decoder for the derived generic rep of my config type, like so:

```hs
newtype Config = Config
  { targets :: Array URL }
derive instance genericConfig :: Generic Config _
instance decodeConfig :: Decode Config where
  decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true }
```

By having an instance of `Decode`, I get access to decodeJSON, which returns an `Except Errors Config`. While I could do more with this type, I chose to do the easiest thing and just "run" the `Except` to get an `Either` to work with:

```hs
  case runExcept decoded of
    Right (Config config) -> do
      -- ...
    Left e -> do
      errorShow e
```

In the case of an error, I show the error and and end, but in the case that I've decoded my config successfully, I'm ready to connect to my database and continue:

```hs
      conn <- newDB "./data"
      for_ config.targets $ (downloadCasts conn)
      closeDB conn
```

`newDB` returns an `Aff Connection` that I can run like before. I then use `for_` to traverse my list of target channels with a function to download "casts" from the page, supplying my database connection to for use. `for_` also throws away the inner values that I'm not concerned with, while `for` would give me the results in a collection.

### Downloading links from a channel

Similar to before, I need to asynchronously get something at this step, which happens to be the HTML I'm working with:

```hs
downloadCasts ::
  forall e.
  DBConnection ->
  String ->
  Aff
    (Program e)
    (Array CastStatus)
downloadCasts conn url = do
  res <- Affjax.get url
```

Then I need to extract the links for the videos I'm going to be looking at. For this, I'm using my [lenient HTML parser](https://github.com/justinwoo/purescript-lenient-html-parser) library:

```hs
getCasts :: HTMLString -> Either ParseError (Array Cast)
getCasts s = do
  tags <- parseTags s
  pure $ foldMap getLinks tags
  where
    -- ...
```

Here, I bind the result of parsing my tags and further work with it by foldMapping to accumulate the inner collections returned from getLinks. On parsing failure, this will just spit out `Left ParserError`. The inner function is defined like so:

```hs
    getLinks (TagOpen (TagName "a") attrs) = do
      case contains (Pattern "yt-uix-tile-link") <$> (getAttr "class" attrs) of
        Just true -> do
          case {title: _, link: _}
            <$> getAttr "title" attrs
            <*> ((<>) "https://www.youtube.com" <$> getAttr "href" attrs)
            of
            Just a -> pure a
            Nothing -> mempty
        _ -> mempty
    getLinks _ = mempty
    getAttr match xs = getValue <$> find matchName xs
      where
        matchName (Attribute (Name name) _) = match == name
        getValue (Attribute _ (Value x)) = decode <<< trim $ x
```

The primary case I'm concerned with is to find an open anchor tag, and then to inspect its attrs to look for a classname. In the case that the classname attribute exists and the classname is found, I can attempt to grab the title and href attributes to prepare a target to download from. If both attributes are found, then I return a singleton list of my download target. On failure, I return an empty list. For any other tag found in the HTML, I'm not interested in the results, and so I return a bunch of empty lists for those also.

Back in my downloadCasts function, I can use getCasts:

```hs
downloadCasts conn url = do
  res <- Affjax.get url
  case getCasts res.response of
    Right casts -> for casts $ downloadCast conn
    Left e -> do
      errorShow e
      pure []
```

In the case that the parse is successful and I get a list of targets back, I'm able to run downloadCast on each.

### Downloading a link

In this step, I check if I've already downloaded the link against my database:

```hs
downloadCast ::
  forall e.
  DBConnection ->
  Cast ->
  Aff
    (Program e)
    CastStatus
downloadCast conn cast = do
  exists <- (\rows -> 1 == length (unsafeCoerce rows)) <$> queryDB conn "SELECT 1 from downloads where link = ?" [cast.link]
```

I use `unsafeCoerce` here because the type of the rows doesn't matter, only its length. I can then use this information to choose whether or not to download the show:

```hs
  case exists of
    true -> pure CastAlreadyDownloaded
    false -> do
      result <- runDownload cast.link
      case result of
        Right _ -> do
          _ <- queryDB conn "INSERT INTO downloads (link, title, created) VALUES ($1, $2, datetime('now'));" [cast.link, cast.title]
          log $ "downloaded " <> cast.title <> " from " <> cast.link
          pure $ CastDownloaded cast
        Left e -> do
          log $ "cast download failed of " <> cast.title <> " " <> show e
          pure $ CastDownloadFailed e cast
```

In the case that I don't already have an entry for having downloaded my link, I run the download and then look inspect the result. If the child process ran without error, I insert the target information into my database and log that I have completed downloading it. The runDownload definition is fairly straightforward:

```hs
runDownload :: forall e.
  URL ->
  Aff
    ( cp :: CHILD_PROCESS
    | e
    )
    (Either Error String)
runDownload url = makeAff \e s -> do
  process <- spawn "youtube-dl"
             [ "-o"
             , "downloads/%(title)s.%(ext)s"
             , "-x"
             , "--audio-format"
             , "mp3"
             , url
             ]
             $ defaultSpawnOptions { stdio = [Just Pipe] }
  onError process $ toStandardError >>> Left >>> s
  onExit process $ const (s $ Right "success?")
```

This creates an Aff which will spawn the `youtube-dl` child process with the appropriate arguments, and on exit, complete the Aff with a `Right String`, and on error, complete the Aff with a `Left String`. I choose not to use the error handler as I consider the error of the child process to be non-critical (I will just print out an error message and continue on).

That's it!

## Conclusion

While nothing too new was shown here, hopefully this has demonstrated that you can write 100% Purescript projects for doing various things in Node with minimal need to think about the Node-level details. Even if you don't know JS, you can write Purescript (and if you know JS quite well, even better)! Isn't that cool?

Let me know what you think on [twitter](https://twitter.com/jusrin00)!

## Links

* Repo https://github.com/justinwoo/ytcasts
* Haskell version https://github.com/justinwoo/ytcasts2
* How to Foreign-Generic https://github.com/justinwoo/purescript-howto-foreign-generic
* Previous post http://qiita.com/kimagure/items/5674e3ae9c87262af762
* Post about the Haskell version http://qiita.com/kimagure/items/0a2f3d60789c646e4426