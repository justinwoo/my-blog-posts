# Type-level Path Params parsed to Records with PureScript

*Edit: I've written a follow-up to this article to write paths from records here: https://qiita.com/kimagure/items/777133d6bbff67e3819d* 

Recently, I looked at some examples of how people handle paths and path parameters commonly in "normal" programming languages. Many of these approaches involved matching on items in an array produced by a glorified string split on `/`, by both guarding on string value matches and by binding segments into variables (of type string) for use. While these seemed okay enough given the various constraints (or lack thereof, ha) of these examples and the languages they were written for, I didn't want much to do with it. I wanted more static guarantees about how this would work:

* Even if I match with string values, how do I use this information for other contexts? e.g. using this type information for both client/server code
* If I bind the parameters to variables and I want this parameter parsed to integer, then do I end up paying a parsing boilerplate penalty on usage sites everywhere?

So in my own experiment in PureScript, instead of matching on a pattern match on an array, I decided to allow myself to use Symbols (type-level strings) to match literal segments and a phantom/proxy type Param to match the parameter segments, parse them to the type that was needed, and build a Record of these parameters using a Symbol label associated to each Param. Static guarantees bonanza!

*Speaking of which, I hope you'll check out [this interview with Simon Peyton-Jones](http://www.cs.cmu.edu/~popl-interviews/peytonjones.html) and the segment about what was surprising about putting Haskell out to the public:*

*"I think it's because people in companies are writing software that they want to still be able to maintain and modify in five years time. As you scale up, and as your ambition about timescale increases, so maybe you'll invest more in the static guarantees you get from a type system, and then you push the type system harder."*

## Path as a collection of literal Segments and Params

To model paths, I needed some kind of heterogeneous collection that wasn't ugly to use in the end. Luckily for me, there already exists such a structure -- Tuples! So I reused the existing Tuple library and provided myself a infix type operator identical to the existing [`/\`](https://pursuit.purescript.org/packages/purescript-tuples/4.1.0/docs/Data.Tuple.Nested) operator, but as a more familiar symbol:

```hs
infixr 6 type Tuple as /
```

For the literal Segments I needed a data type that would provide a place for my Symbols, so I reused the existing [String Proxy type](https://pursuit.purescript.org/packages/purescript-symbols/3.0.0/docs/Data.Symbol#t:SProxy) and renamed it to not bog down my screen:

```hs
type S = SProxy
```

And for Params, I didn't know of any existing type that would contain both my Symbol and my type together, so I made a new phantom type to hold this information:

```hs
data Param (label :: Symbol) ty = Param
```

And that's about it! Now I could set it up so I would write my type-level paths:

```hs
type MyRoute = S "hello" / S "world" / Param "id" Int / Param "name" String
```

## Handling errors and parsing Params

As usual, as I want to collect multiple errors from multiple branches, I'm using the `Except` type with the left side being a `NonEmptyList error`. The reason I use a non-empty list for errors is that I will never have an empty list of errors, and I should get guarantees from my types about that.

Using this, I wrote a data type for my errors and the code to parse a given parameter:

```hs
data BadTime
  = SymbolMatchError String
  | ParamParseError String

type BadTimes = NonEmptyList BadTime

class ParseParam a where
  parseParam :: String -> Except BadTimes a

instance stringParseParam :: ParseParam String where
  parseParam s = pure s

instance intParseParam :: ParseParam Int where
  parseParam s =
    case fromNumber $ readInt 10 s of
      Just a -> pure a
      Nothing ->
        throwError <<< pure <<< ParamParseError $
          "could not parse " <> s <> " into integer"
```

## Parsing the URL using our Tuple

When parsing my route, I knew that using the type information, I wanted to get out two things from handling each section and the overall URL:

* A Record.Builder that would give me a function for building a record with my parsed parameters
* The remaining string path after applying each section/the whole URL.

The interesting thing about [Record.Builder](https://pursuit.purescript.org/packages/purescript-record/0.2.5/docs/Data.Record.Builder#t:Builder) is that they can be used to build up a complete record from a starting point, with no intermediate representation costs. They also actually compose together through their [Semigroupoid](https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Control.Semigroupoid#t:Semigroupoid) instances, providing composable momrphisms, and provide an identity operation through [Category](https://pursuit.purescript.org/packages/purescript-prelude/3.1.0/docs/Control.Category#t:Category). While many claim many operations of their programs are "composable", in this case, Builders are legitimately composable.

And so I can define my type class for the Parse URL Implementation for a given type and the from-to row types, with a functional dependency that simply uses the type to look up instances:

```hs
class ParseURLImpl xs (from :: # Type) (to :: # Type)
  | xs -> from to where
  parseURLImpl ::
       Proxy xs
    -> String
    -> Except BadTimes
         { builder :: Builder (Record from) (Record to)
         , remaining :: String
         }
```

Then writing the instances doesn't actually take too much. For the Tuple case, I run the left side, use the remaining to run the right, and then compose the builders together and reutrn the right:

```hs
instance tupleParseURL ::
  ( ParseURLImpl left from' to
  , ParseURLImpl right from from'
  ) => ParseURLImpl (left / right) from to where
  parseURLImpl _ s = do
    left <- parseURLImpl (Proxy :: Proxy left) s
    right <- parseURLImpl (Proxy :: Proxy right) left.remaining
    pure $ { builder: left.builder <<< right.builder, remaining: right.remaining }
```

For the literal segments, I strip the prefix of `/` and the Symbol segment reflected to string from the path string and return a builder of `id`, since there are no parameters to be added to the params record.

```hs
instance segmentParseURL ::
  ( IsSymbol segment
  ) => ParseURLImpl (SProxy segment) from from where
  parseURLImpl _ s =
    case stripPrefix (Pattern $ "/" <> segment) s of
      Nothing ->
        throwError <<< pure <<< SymbolMatchError $
          "could not strip segment " <> segment <> " from path " <> s
      Just remaining ->
        pure { builder: id, remaining }
    where
      segment = reflectSymbol (SProxy :: SProxy segment)
```

Then for the params, I split off the head of my path string and used my ParseParam class from earlier. Then I used this to return a builder that would insert this value with the associated label to my record.

```hs
instance paramParseURL ::
  ( IsSymbol label
  , ParseParam ty
  , RowLacks label from
  , RowCons label ty from to
  ) => ParseURLImpl (Param label ty) from to where
  parseURLImpl _ s = do
    split' <- maybe
                (throwError <<< pure <<< ParamParseError $
                   "could not handle url param segment " <> s)
                pure
                split
    value <- parseParam split'.before
    pure { builder: Builder.insert labelP value, remaining: split'.after}
    where
      labelP = SProxy :: SProxy label
      label = reflectSymbol labelP
      s' = drop 1 s
      split = case indexOf (Pattern "/") s' of
        Just idx -> splitAt idx s'
        Nothing -> pure { before: s', after: "" }
```

And that's actually everything! Not too much involved when just reusing everyone else's work :)

I then just wrap this up in a more convenient top-level function:

```hs
parseUrl :: forall to xs
   . ParseURLImpl xs () to
  => Proxy xs
  -> String
  -> Either BadTimes (Record to)
parseUrl p s = runExcept do
  result <- parseURLImpl p s
  pure $ Builder.build result.builder {}
```

*And yes, this doesn't quite check if the rest of the path hasn't been used, and doesn't handle query strings or something, but this is a demo. This could always be improved greatly in the future.*

*In retrospect, if I wanted to make this a more reliable library, I should parse URLs into a list or something. But this is just a demo to inspire people to make their own solutions (or flood my own demo with PRs to improve it).**

## Usage

The usage ends up being quite simple:

```hs
type MyRoute = S "hello" / S "world" / Param "id" Int / Param "name" String

myRouteP :: Proxy MyRoute
myRouteP = Proxy

testUrl :: String
testUrl = "/hello/world/1/joe"

main :: Eff _ Unit
main = runTest do
  suite "LA Galbi" do
    test "works properly" do
      case parseUrl myRouteP testUrl of
        Left e -> failure $ "oops: " <> show e
        Right result ->
          assert "says hi joe #1" $
            "hi " <> result.name <> " #" <> show result.id == "hi joe #1"
```

And so you'll see that the result of the success case doesn't require any annotations or anything as those are already available from the function and its instances, so result is a normal record with the fields `( id :: Int, name :: String )` as expected.

## Conclusion

Hopefully this has shown you that there are very nice ways to do path parameter parsing with PureScript (and Haskell) that aren't too much work to implement, for which you can then extend with your own code for building requests, doing client code gneeration, etc. And most importantly, instead of writing a bunch of value-level code, we can write just our types first and get all the generic value-level operations derived for us.

And yes, in many ways this is just a simple/stupid way to do what [PureScript-Trout](https://github.com/owickstrom/purescript-trout), [Servant](http://haskell-servant.readthedocs.io/en/stable/tutorial/ApiType.html), and [Yesod](https://www.yesodweb.com/book) do, but for various reasons, you might want different/simpler guarantees than what those will provide you.

## Links

* This demo https://github.com/justinwoo/purescript-la-galbi