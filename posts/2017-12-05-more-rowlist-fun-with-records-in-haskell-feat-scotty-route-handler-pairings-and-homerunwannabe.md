# More "RowList" fun with Records in Haskell feat. Scotty Route/Handler pairings & HomeRunWannabe

Before, I wrote about how I was using an approximation for RowToList using GHC8 Generics and Generic-Lens [here](https://qiita.com/kimagure/items/6a9764966edd6cef497d). Since then, I've done a few more experimentations and have found it very fun to work with Haskell records.

Hopefully, this can show you that you don't have to scream about not having row types or extensible records if you know some details about what you want to get done. That's right -- I'm fairly sure you can do most of what you want with Haskell records, GHC8 Generics, and some helper type families and libraries like Generic-Lens, and I'm the kind of madman who comes up with [all kinds of strange uses for row types](https://github.com/justinwoo/awesome-rowlist).

## Refresher

I use this bit from [kcsongor](https://github.com/kcsongor) to convert a generic rep of a record type into a "RowList", which I work with as a list of tuples of `Symbol` and `*`:

```hs
-- GRowToList from kcsongor
type family GRowToList (r :: * -> *) :: [(Symbol, *)] where
  GRowToList (l :*: r)
    = GRowToList l ++ GRowToList r
  GRowToList (S1 ('MetaSel ('Just name) _ _ _) (Rec0 a))
    = '[ '(name, a) ]
  GRowToList (M1 _ m a)
    = GRowToList a
  GRowToList U1 = '[]
  
-- utils for appending RowLists
type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
```

With this, I'm ready to write just about any type class which primarily resolves instances using the RowList. *This is what I do with row types and RowToList in a nutshell.*

## Example 1: Fun Scotty Routing Demo

I originally posted about my PureScript solution [here](https://qiita.com/kimagure/items/bb9bd3e4ffe1bba4c214) about how I built up a record of Routes that I shared between my client and my server, and this was used to ensure that I had handlers for every route. As the original was implemented as a pairwise RowList type class, I knew this couldn't be much work.

### Types

In my model, I have to request types to handle: GET and POST requests.

```hs
data GetRequest
data PostRequest
```

In most of my own apps, I have a simple Route model that associates four things:

* method
* request type
* response type
* symbol URL

Because my requests look like `POST "AddApples 2" "Success YouGotItCap'n "/api/apples"`, and there is only one valid combination here. I do not want to get oranges back from `/api/apples`. So this gets modeled as such:

```hs
data Route method req res (url :: Symbol) = Route
```

And I add some type aliases to make life easier for me, where a GET request does not have an associated request body (and thus, gets Void):

```hs
type GetRoute = Route GetRequest Void
type PostRoute = Route PostRequest
```

So yeah, that's about it for my types. Just a glorified way for me to say that I don't wear swim trunks with blazers and you can't just ship random things/characters together.

### Iterating routes and handlers

There's essentially five things we need to iterate our record of routes and handlers:

1. Our routes record type
2. Our routes RowList
3. Our handlers record type
4. Our handlers RowList
5. Whatever monad we're evaluating to

For which we'll just lazily define a function that takes the four params and returns a unit in our monad, since that's what registering a route comes down to in Scotty.

```hs
-- go through the records pairwise and register each handler
class RegisterRoutes
    routes
    (routesL :: [(Symbol, *)])
    handlers
    (handlersL :: [(Symbol, *)])
    m
  where
    registerRoutesImpl :: forall
       . Monad m
      => routes
      -> Proxy routesL
      -> handlers
      -> Proxy handlersL
      -> m ()
```

As always, our empty list instance head is a no-op. Just to be sure though, I require that both RowLists are empty to make sure we don't have one being longer than the other.

```hs
instance RegisterRoutes routes '[] handlers '[] m
  where
    registerRoutesImpl _ _ _ _ = pure ()
```

Then in the case of our cons instances, we use another type class we define for the actual registration of the route and conveniently pull out `HasField'` from Generic-Lens, which lets us pull out a name from a record to get the individual route/handler.

```hs
instance
  ( RegisterRoutes routes routesTail handlers handlersTail m
  , HasField' name routes route
  , HasField' name handlers handler
  , RegisterHandler route handler m
  ) => RegisterRoutes
         routes
         ('(name, route) ': routesTail)
         handlers
         ('(name, handler) ': handlersTail)
         m
  where
    registerRoutesImpl routes _ handlers _ = do
        registerHandlerImpl route handler
        registerRoutesImpl
          routes
          (Proxy :: Proxy routesTail)
          handlers
          (Proxy :: Proxy handlersTail)
        pure ()
      where
        route = getField @name routes
        handler = getField @name handlers
```

Register the handler, and then register all the rest by passing down the tail. The RegisterHandler definition ends up being some typical instance head matching:

```hs
-- register each handler, to the route method and concrete monad used
class RegisterHandler route handler m
  where
    registerHandlerImpl :: route -> handler -> m ()

instance
  ( KnownSymbol url
  , Show res
  ) => RegisterHandler
         (Route GetRequest Void res url)
         (IO res)
         ScottyM
  where
    registerHandlerImpl _ handler =
        get (capture path) $ do
          res <- liftAndCatchIO handler
          text . pack $ show res
      where
        path = symbolVal (Proxy :: Proxy url)
```

To then expose a human-friendly API, I define a function that takes in the routes and handlers and takes care of all the dirty work:

```hs
registerRoutes :: forall routes routesL handlers handlersL m
   . Monad m
  => Generic routes
  => Generic handlers
  => routesL ~ GRowToList (Rep routes)
  => handlersL ~ GRowToList (Rep handlers)
  => RegisterRoutes
       routes
       routesL
       handlers
       handlersL
       m
  => routes
  -> handlers
  -> m ()
registerRoutes routes handlers =
  registerRoutesImpl
    routes
    (Proxy :: Proxy routesL)
    handlers
    (Proxy :: Proxy handlersL)
```

### Usage

With all this done, the actual usage comes down to some very routine looking code:

```hs
data MyRoutes = MyRoutes
  { home :: GetRoute Bool "/"
  , hello :: GetRoute Int "/hello"
  , bye :: GetRoute String "/bye"
  } deriving (Generic)

myRoutes :: MyRoutes
myRoutes = MyRoutes
  { home = Route
  , hello = Route
  , bye = Route
  }

data MyHandlers = MyHandlers
  { home :: IO Bool
  , hello :: IO Int
  , bye :: IO String
  } deriving (Generic)

myHandlers :: MyHandlers
myHandlers = MyHandlers
  { home = pure True
  , hello = pure 1
  , bye = pure "bye"
  }
  
main :: IO ()
main = scotty 3001 $ do
  registerRoutes myRoutes myHandlers
```

## Example 2: HomeRunWannabe

This is an approximation of my PureScript library that I originally wrote about [here](https://qiita.com/kimagure/items/eeb40541fc56b8dba2cc). The purpose of the PureScript library is to provide a way to specify a row of validations to be performed and to get either a list of the keys that had failed as Variants (such that you could correctly statically match them) or as a value with evidence of the validations performed, such that it should be impossible to call functions that have constraints for certain validations having been performed.

This approximation does not come with a list of variants of symbol proxies, because 1) I did not want to write my own, 2) it's a smaller point that doesn't matter as much for a demo. If one were to make this a full-fledged library, this might end up being a priority, but for now, it has been simplified as a String.

### Rules

The library works in a fairly simple way, in that the main unit is a rule: a type that can carry any amount of information needed to validate a value, and another type which will contain the values to be validated. This ends up being a simple definition:

```hs
-- Validate Rule, where a given rule can also be used to validate any value type
class ValidateRule rule a where
  validateRuleImpl :: Proxy rule -> a -> Bool
```

For convenience, I have a type alias for validated values that carry evidence, using Const:

```hs
-- Validated Structure
type ValidatedValue rules value = Const value (Proxy rules)
```

### Checking Rules

In this case, there is only the simple RowList to iterate, with our value being provided for being validated. While we need to combine these validations, we don't need to actually combine the value as we will just have the one value to return, so the instance method can return unit.

```hs
-- Check the validations defined
class CheckRules (rulesL :: [(Symbol, *)]) a where
  checkRulesImpl :: Proxy rulesL -> a -> Validation [String] ()
```

And as usual, the nil instance is a no-op:

```hs
instance CheckRules '[] a where
  checkRulesImpl _ _ = pure ()
```

For our cons instance, there are a couple of things to work from:

* We need to constrain the name of the RowList items to use the string value to return errors
* We need to use our ValidateRule class from before to validate a rule for the given value
* We need to iterate the rest of the row

Another thing that comes up is that the Validation libraries in Haskell are default monadic, whereas I need the applicative behavior to accumulate errors (see my post about Validation in Pure script [here](https://qiita.com/kimagure/items/f75befebdd37f6e8879f).

To get applicative appending of errors, I mapped the append operation to the application of the two sides. And so the instance looks like this:

```hs
instance
  ( KnownSymbol name
  , ValidateRule rule a
  , CheckRules tail a
  ) => CheckRules ('(name, rule) ': tail) a where
  checkRulesImpl _ x = (<>) <$> curr <*> rest
    where
      curr = if validateRuleImpl (Proxy @rule) x
        then pure ()
        else Failure . pure $ symbolVal (Proxy @name)
      rest = const () <$> checkRulesImpl (Proxy @tail) x
```

To expose this function in a more human-usable way, I then wrapped the function and mapped the tagged value to the Validation.

```hs
-- exposed function
checkRules :: forall a rules rulesL
   . Generic rules
  => rulesL ~ GRowToList (Rep rules)
  => CheckRules rulesL a
  => Proxy rules
  -> a
  -> Validation [String] (ValidatedValue rules a)
checkRules _ a =
  const (Const a) <$> checkRulesImpl (Proxy @rulesL) a
```

### Usage

First, I defined some validations I cared about:

```hs
data BeginsWith (s :: Symbol)
data Contains (s :: Symbol)
data EndsWith (s :: Symbol)

instance KnownSymbol prefix => ValidateRule (BeginsWith prefix) String where
  validateRuleImpl _ input = symbolVal (Proxy @prefix) `isPrefixOf` input
instance KnownSymbol substring => ValidateRule (Contains substring) String where
  validateRuleImpl _ input = symbolVal (Proxy @substring) `isInfixOf` input
instance KnownSymbol suffix => ValidateRule (EndsWith suffix) String where
  validateRuleImpl _ input = symbolVal (Proxy @suffix) `isSuffixOf` input
```

This way, I can validate strings for beginning, containing, and ending with some pattern.

I then defined what validations I wanted performed in a record type:

```hs
data FileNameValidations = FileNameValidations
  { group :: BeginsWith "[BananaSubs]"
  , resolution :: Contains "[720p]"
  , extension :: EndsWith "mkv"
  } deriving (Generic)
```

Importantly, I have to derive generic here for my CheckRules type class to work.

I then defined a function that could only be called with "group" had been validated to begin with `"[BananaSubs]"`.

```hs
onlyOnGroupBananaSubs :: forall rules
  . HasField' "group" rules (BeginsWith "[BananaSubs]")
 => ValidatedValue rules String
 -> String
onlyOnGroupBananaSubs (Const s) = "subbed by BananaSubs: " ++ s
```

With this, I was able to validate some strings using these rules and correctly only use the function on the validated branch not only because of the validation, but because the `HasField'` constraint correctly made sure that the `FileNameValidations` contained `group :: BeginsWith "[BananaSubs]"`. Like so:

```hs
checkRules' :: String -> IO ()
checkRules' s =
  case checkRules (Proxy @FileNameValidations) s of
    Success x -> putStrLn $ onlyOnGroupBananaSubs x
    Failure e -> putStrLn $ "failed to validate on keys: " ++ intercalate ", " e

main :: IO ()
main = do
  checkRules' "[AbogadoSubs] Tom Kha Gai [720p].avi"
  checkRules' "[BananaSubs] Phad Cha [720p].mkv"
  -- output:
  -- failed to validate on keys: group, extension
  -- subbed by BananaSubs: [BananaSubs] Phad Cha [720p].mkv
```

## Conclusion

I hope this was able to show you that there are many interesting things you can do with normal Haskell records with GHC Generics, type families, and libraries like Generic-Lens. In particular, the type family to convert record fields into a RowList provided many of the capabilities normally required of using `RowToList` in PureScript, and `HasField'` from Generic-Lens provides a way to work with record types like `RowCons` in PureScript.

## Links

* Fun Scotty Demo https://github.com/justinwoo/fun-scotty-routing-demo
* Original PureScript Route Handling post: https://qiita.com/kimagure/items/bb9bd3e4ffe1bba4c214
* Home Run Wannabe Demo https://github.com/justinwoo/home-run-wannabe
* Original PureScript Home-Run-Ball post: https://qiita.com/kimagure/items/eeb40541fc56b8dba2cc
* Generic-Lens https://github.com/kcsongor/generic-lens