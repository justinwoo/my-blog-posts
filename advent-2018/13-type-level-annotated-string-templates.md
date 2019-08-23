Previously, I covered my post about well-typed paths using Symbols with formatting parameters, such that we can derive a parsing function from a Symbol proxy like so:

```hs
-- inferred type:
parseHelloURL :: String -> Either String { name :: String, age :: String }
parseHelloURL = parseURL (SProxy :: SProxy "/hello/{name}/{age}")
```

But really, we might like to read `age` here as an `Int`, and we could provide this information as a type annotation. And we can!

## "Parsing type-level strings to extract types"

In this post, I go through how we can modify the previous implementation to conditionally parse out a type annotation from our Symbol, by using the same techniques for parsing piecewise with instance chain groups as done in Record-Format and other libraries.

<https://qiita.com/kimagure/items/6729a5d55ab99bcee8ec>

This way, we can instead work with URLs that have their types annotated like so:

```hs
parseHelloURL :: String -> Either String { name :: String, age :: Int }
parseHelloURL = parseURL (SProxy :: SProxy "/hello/{name:String}/{age:Int}")
```

## "Simple Routing based on parsing type-level strings"

There's also a follow-up post to this that applies classic RowToList techniques to allow for a row of these routes to be tried to product an Either of no matches and a Variant of the routes:

<https://qiita.com/kimagure/items/5c3f3fcb898e480c56be>
