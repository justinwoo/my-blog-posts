# Making a simple node program with Purescript

For whatever reason, I decided at 27:00 Friday that I should make a program that would get audio from videos of a Youtube channel. This article will go over the details on why and how I made this with Purescript (and some FFI).

## Why

Some Youtubers basically make podcasts with their videos, and so while they make gestures to better communicate their topics, the audio is the only content you'd really be interested in. I knew I could use youtube-dl to download the audio from these, and I could just scrape the channel page HTML with cheerio which I could get by fetching.

I knew I could do this using these node libraries, and that I wanted to use Purescript to model my problem and help me write code that would be mechanically correct. Of course, this correctness guarantee does not fully extend to my Foreign Function Interface (FFI) code, but being able to guarantee that the rest of the program would work correctly is amazing.

## How

### Setup

Of course, I had to use a combination of tools. I used pulp to generate my project, and bower to install the modules: `pulp init ytcasts && cd ytcasts && bower i -S purescript-aff purescript-affjax purescript-node-fs-aff`

Then in my project I had to make sure my node dependencies would be in order: `npm init && npm i -S cheerio sqlite3 xhr2`.

As you can see, I decided to use sqlite3 to keep track of what files were downloaded, and I had to install xhr2 so that purescript-affjax could use it for XHR.

I set a simple schema for my DB: `sqlite3 ./data`

```sql
CREATE TABLE downloads (link varchar(20) primary key, title varchar, created datetime);
```

### Code

#### "Affects"

I use [purescript-aff](https://github.com/slamdata/purescript-aff) for asynchronous controls. This allows me to wrote code using do blocks for what looks like synchronous code, running each asynchronous effect/"Aff" and getting the value back. For example, to use my sqlite database, I need to manage async processes in creating the database connection, running a query and returning its results, and closing the connection. This looks like this:

```haskell
launchAff do
  conn <- newDB "./data"
  rows <- queryDB conn "SELECT 1 from downloads where link = ?" [cast.link]
  closeDB conn
```

And these are backed up by definitions that look like this:

```haskell
newDB :: forall e. FilePath -> Aff (db :: DBEffects | e) DBConnection
newDB path = makeAff (\e s -> runFn2 _newDB path s)
```

This looks very complicated at first, but we can just break down the parts we need to understand. `newDB` is a function where you pass in a filepath (for our sqlite database file), which will return an Aff with the effects of DBEffects named "db" and any other effects in our system yielding DBConnection.

The definition uses `makeAff` to turn the implementation `_newDB` into an Aff by applying the path argument and a success callback.

Of course, we then need to look at how `DBEffects`, `DBConnection`, and `_newDB` are defined.

### foreign data types

The data types are actually part of the outer system, so we define them as foreign imports:

```haskell
foreign import data DBConnection :: *
foreign import data DBEffects :: !
```

The `*` is a kind of all types with values. In this case, as a single `*`, this just means that DBConnection is a data type constructed on its own, but you might remember seeing that `List` has the kind `* -> *`, which means that a type argument is supplied to create a type, like `List Int`.

The `!` is a kind for effects, allowing me to identify effects in my program of DBEffects. This is useful for marking which functions will have which effects.

### FFI

```haskell
foreign import _newDB :: forall e.
  Fn2
    FilePath
    (DBConnection -> Eff (db :: DBEffects | e) Unit)
  (Eff (db :: DBEffects | e) Unit)
```

Here I am importing `_newDB` and using `Fn2` to mark that my foreign function takes two arguments, which is why above I have `runFn2`, for marking that I am using an uncurried function with two arguments. The second argument is for a callback which will take DBConnection as an argument and produces an effect. The actual implementation on the JS side looks like this:

```javascript
exports._newDB = function (filename, cb) {
  return function () {
    cb(new sqlite3.Database(filename))();
  }
}
```

So we export the function, and it has two arguments. It returns a function for what will be then executed for these provided arguments, and the callback is also called with the resolving value and then that result is called again for running the effect. It can be a little confusing, but this extra reading might be very helpful: https://leanpub.com/purescript/read#leanpub-auto-the-foreign-function-interface

### Reading my config file

With the purescript-node-fs-aff library I installed, it's very easy to read a text file:

```haskell
type Config =
  { targets :: Array URL }

foreign import parseConfig :: String -> Config

main = launchAff do
  config <- parseConfig <$> readTextFile UTF8 "./config.json"
```

Okay, so config is what's being set when everything right of `<-` runs, but what is on the right? `<$>` is the operator alias for `map` (or `fmap` in Haskell), which is implemented for all types implementing the `Functor` typeclass. While I could tell you that functors are homomorphisms between categories, the better explanation is that this is like your normal `Array#map` method, but with a twist: instead of just working on `Array a`, it works for any `f a` where `f` has an implementation for `Functor` defining `map`.

So by mapping `parseConfig` to the result of my config file's text, I'm able to get the configuration for my program, essentially turning `Aff String` into `Aff Config`.

### Fetching my casts

We can get the gist of my program just by looking at what I have in my main function and the signatures of my other functions, so let's do that first:

```haskell
type Cast =
  { title :: String
  , link :: URL
  }

data CastStatus
  = CastAlreadyDownloaded
  | CastDownloaded Cast

downloadCasts :: forall e.
  DBConnection ->
  String ->
  Aff (ajax :: AJAX, fs :: FS, db :: DBEffects | e) (Array CastStatus)

-- launch the Affs in my program
main = launchAff do
  -- get my config
  config <- parseConfig <$> readTextFile UTF8 "./config.json"

  -- connect to the DB
  conn <- newDB "./data"

  -- get target statuses for each target by downloading the casts in it
  targetStatuses <- for config.targets $ downloadCasts conn

  -- for each target, just run the Aff for reporting cast statuses
  for_ targetStatuses $ reportTargetStatus

  -- disconnect from the DB
  closeDB conn

  where
    -- report the status for each cast
    -- note that traverse and for are the same but with arguments flipped
    reportTargetStatus =
      traverse_ reportStatus

    -- report the status
    reportStatus status =
      case status of
        -- we don't care if it's already downloaded
        CastAlreadyDownloaded -> pure unit

        -- if it was downloaded just now, let's see the info
        CastDownloaded cast -> log $ "downloaded " <> cast.title <> " from " <> cast.link
```

And that's really the bulk of it! The details that don't matter as much are in the implementations:

```haskell
type HTMLString = String

foreign import getCasts :: HTMLString -> Array Cast

downloadCasts conn url = do
  -- fetch the page
  res :: AffjaxResponse String <- Affjax.get url

  -- get the casts out of the page
  let casts = getCasts res.response

  -- download each of the casts in the page
  for casts $ downloadCast conn
```

in which getCasts looks like this:

```javascript
exports.getCasts = function(string) {
  var $ = cheerio.load(string);
  var casts = [];
  $('.channels-browse-content-grid a.spf-link').each(function () {
    var $this = $(this);
    casts.push({
      title: $this.text(),
      link : 'https://www.youtube.com' + $this.attr('href')
    });
  });
  return casts;
}
```

And then for downloading each cast:

```haskell
downloadCast conn cast = do
  -- check if we already have this cast in our DB
  exists <- (\rows -> 1 == length rows) <$> queryDB conn "SELECT 1 from downloads where link = ?" [cast.link]

  case exists of
    -- "return" that we have already downloaded this
    -- that is, take the value and wrap it up in Aff
    true -> pure CastAlreadyDownloaded

    false -> do
      -- download it now!
      runDownload cast.link

      -- add it to the DB that we downloaded it
      queryDB conn "INSERT INTO downloads (link, title, created) VALUES ($1, $2, datetime('now'));" [cast.link, cast.title]

      -- "return" that we've downloaded it
      pure $ CastDownloaded cast
```

As you can see, my `downloadCast` function is a bit of a facade, just like how you might have implemented something similar with Promises in JS by either returning your cached value or returning a new promise that will do the work and cache your result.

`_runDownload` looks like this:

```javascript
exports._runDownload = function (url, cb) {
  return function () {
    console.log('downloading ', url);
    exec('youtube-dl -o downloads/%\\(title\\)s.%\\(ext\\)s -x ' + url, function (err) {
      if (err) {
        throw err;
      } else {
        cb()();
      }
    });
  }
}
```

## Conclusion

Hopefully I've convinced you now that you can use existing libraries with Aff and/or you can implement things in FFI to write node programs and that this language is not actually hard. Give it a try sometime -- it'll be fun.

The complete repo can be found here: https://github.com/justinwoo/ytcasts/

Please let me know if you have corrections/suggestions/feedback: [@jusrin00](http://twitter.com/jusrin00). Thanks for reading!

*This post was written 25:00-27:00 Saturday and may not make sense in some places.*

## Links

* My repo: https://github.com/justinwoo/ytcasts/
* Purescript By Example: https://leanpub.com/purescript/read
* purescript-aff: https://github.com/slamdata/purescript-aff