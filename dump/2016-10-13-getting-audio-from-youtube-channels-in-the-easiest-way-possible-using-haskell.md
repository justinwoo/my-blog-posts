---
title: Getting audio from Youtube channels in the easiest way possible using Haskell
tags: Haskell
author: kimagure
slide: false
---
After visiting a Haskell conference as a not-really-yet-a-Haskeller, I thought it would be fun to try writing my [ytcasts](http://qiita.com/kimagure/items/5674e3ae9c87262af762) program in Haskell. In this post, I'll go over some things I used and how I wrote a program to use `youtube-dl` to download audio from videos on a Youtube channel in Haskell.

## What

To get the audio from Youtube videos, one can use [youtube-dl](https://rg3.github.io/youtube-dl/) to do so. By scraping video URLs from a given Youtube channel, we can download them and listen to them whenever we want, however we want.

## How

My program does the following:

1. Read a `config.json` file to know what channels to scrape
1. Opens a connection to a SQLite database containing records of previous downloads
1. Scrape each channel for video links
1. For every link, check if it has already been downloaded by checking the database, and run `youtube-dl` on the url if it has not
1. Close the database connection once finished

The main function comes down to these lines:

```haskell
main :: IO ()
main = do
  config <- parseConfig <$> readFile "config.json"
  case config of
    Right Config {targets} ->
      bracket (open "data") close $
        for_ targets . downloadCasts
    Left errMsg ->
      putStrLn $ "Error parsing config.json: " ++ errMsg
```

Let's expand on this below.

### Reading in Config

First, I wrote the types for what I wanted and used some automatic derivation helpers to help me automatically get JSON parsing implementations. To accomplish this, I use some [language pragmas](https://wiki.haskell.org/Language_Pragmas) to help me out, which are used to tell the compiler to turn on some features that I'll be using in this file.

```haskell
-- Automatic derivation of "any class" requires this language pragma:
{-# LANGUAGE DeriveAnyClass #-}
-- Automatic derivation of Generic requires this language pragma:
{-# LANGUAGE DeriveGeneric #-}

newtype URL = URL String
  deriving (Generic, Show, FromJSON)

data Config = Config
  { targets :: [URL]
  } deriving (Generic, Show, FromJSON)
```

I used `newtype` here to create a new type "URL" so that the String value would never be used alone, so that I couldn't accidentally pass URLs to a function that needed video titles or something.

Before even worrying about reading the file, I first wanted to make sure I could take some kind of JSON string representation and parse it. To do so, I used [Aeson's](https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html) `eitherDecode`. As this works on `ByteString`s, I needed to use an appropriate `readFile` function.

```haskell
parseConfig :: ByteString -> Either String Config
parseConfig = eitherDecode -- from Data.ByteString.Lazy

main = do
  config <- parseConfig <$> readFile "config.json"
  case config of
    Right Config {targets} ->
      ...
    Left errMsg ->
      putStrLn $ "Error parsing config.json: " ++ errMsg
```

In the "IO context" of my do-block, I'm able to bind to `config` the result of mapping (`<$>`) my config parsing function to my file reading function. (This works because `IO` has an instance of `class Functor`, so I can "lift" my function `ByteString -> Either String Config` into it to make a `IO ByteString -> IO (Either String Config)` function using the map function defined `(<$>) :: Functor f => (a -> b) -> f a -> f b`)

Then I can match the case when the decoding works correctly to the rest of the program or exit, printing the error message I get from decoding the JSON from the contents of `config.json`.

### Opening and Closing and Database connection and downloading the videos

There's a nice utility function [`bracket`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Exception-Base.html#v:bracket) from `Control.Exception.Base` which will take a function for the connection, a function to close it, and one for the operation that should be performed with it, like so:

```haskell
bracket (open "data") close $
  for_ targets . downloadCasts
```

Using `for_`, we can take the target channel URLs and run an operation on each of them, with `downloadCasts` being defined as such:

```haskell
downloadCasts :: Connection -> URL -> IO ()
downloadCasts conn url = do
  casts <- fetchCasts url
  for_ casts (downloadCast conn)
```

### Scraping each channel for links

My `fetchCasts` function is defined like so:

```haskell
fetchCasts :: URL -> IO [Cast]
fetchCasts (URL url) =
  extractCasts . parseTags <$>
    readProcess "curl" ["-s", "--get", url] ""
```

I couldn't figure out how to get TLS handshakes to youtube.com to work (google.com and every other website worked fine), so I resorted to the easiest thing possible: spawn a `curl` child process and just get the page that way. I'd like to eventually get this right, but when only youtube.com fails, I get desperate.

Like before, I mapped some functions to first parse the tags using [TagSoup](https://hackage.haskell.org/package/tagsoup), and then to extract the casts using the parsed information. TagSoup was especially nice since really, all I needed to do was get all my tags in a flat list and bind/flatMap for the information I needed.

Let's back up and look at our `Cast` type is:

```haskell
newtype Title = Title String

data Cast = Cast
  { title :: Title
  , link :: URL
  }
```

Like before, I am wrapping my title string in a newtype to readily know where it should go, and then using it as a property in my `Cast` data type. The extraction then looks like this:

```haskell
extractCasts =
  (=<<) matchA
matchA tag@(TagOpen "a" _) = do
  classNames <- words <$> fromAttrib' "class" tag
  link' <- fromAttrib' "href" tag
  title' <- fromAttrib' "title" tag
  if "yt-uix-tile-link" `elem` classNames
    then pure Cast
      { title = Title title'
      , link = URL ("https://www.youtube.com" ++ link')
      }
    else mempty
matchA _ =
  mempty
fromAttrib' attr tag =
  case fromAttrib attr tag of
    "" -> mempty
    x -> pure x
```

This may seem heavy at first, but it's actually only doing some basic things. `extractCasts` is defined as binding my `matchA` function, which is defined by matching a `Tag` from TagSoup (using `@` to retain an identifier to the whole tag) to the case where it has been constructed with `TagOpen` with "a" as its first argument. This this, I am effectively matching all `<a>` tags.

In the body of this, I am defining a `fromAttrb'` that uses `fromAttrib` from TagSoup, but "fails early" to return nothing in the case that I have nothing for the attribute. In the case that I do have things, it then continues. `words` simply splits the classnames I get from the `class` tag. I then check if the anchor tag I've matched has `yt-uix-tile-link`, and create a `Cast` for it and bind it. Otherwise, it is thrown away and not bound to my list.

By the end of this, I will have either `[]` (unlikely, but maybe, if Youtube changes their  markup) or `[cast, cast2, ...]`.

### Check and download each cast

With this list of casts, I can now download my casts, which is defined simply as

```haskell
downloadCast :: Connection -> Cast -> IO ()
downloadCast conn Cast {title = Title title', link = URL link'} = do
  exists <-
    not . null <$>
    (query conn "SELECT 1 FROM downloads WHERE link = ?" (Only link') :: IO [Only Int])
  unless exists $ do
    putStrLn $ "downloading " ++ title' ++ " from " ++ link'
    void $ readProcess "youtube-dl"
      [ "-o"
      , "downloads/%(title)s.%(ext)s"
      , "-x"
      , "--audio-format"
      , "mp3"
      , link'
      ] ""
    execute conn "INSERT INTO downloads (link, title, created) VALUES (?, ?, datetime('now'));"
      (link', title')

```

First, I check in my database if I have an entry for this cast already using the link. `not . null` lets me easily negate the check to see if this is empty. Then I use `unless` to check the condition and only run the rest.

I print that I am downloading the file, then I spin off a `youtube-dl` process with the arguments to make it download the audio as mp3 accordingly. I use `void` to disregard the result. Then I add a row to mark that this has been downloaded.

With this, we have everything set up to `((((conditionally download audio) from videos) of channels) in my config)`.

## Conclusion

We've gone through a whole bunch of stuff in this post, and it might not make the most sense (especially since I haven't been able to write everything in the clearest, simplest way possible), but I hope this gives you some idea of what writing Haskell code can be like.

Thanks for reading! Questions? Corrections? Comments? Please let me know on [twitter](https://twitter.com/jusrin00)!

## Links

* This code: https://github.com/justinwoo/ytcasts2
* My other post on writing a Telgram bot with Purescript: http://futurice.com/blog/making-a-weather-telegram-bot-in-purescript

