In previous posts, I wrote about using type-level parsing for URLs. While that's nice and all, it's not necessarily what seems readily usable to solve some problem. Luckily, I have a worse problem to talk about, but with a nice solution.

Often, I use SQLite from PureScript by using the sqlite3 library. This library lets me supply parameters by naming them, and providing a record that has properties where the labels match the ones in the query, so that the parameters are applied to the query. This is nice, but in typical usage, it's untyped in that I can easily forget to specify properties and cause my query to always fail.

Well, I can solve that, can't I? I have a static query that I'm going to be using the vast majority of the time, so I can put those Symbol parsing skills to use.

## "Well-typed parameterized SQLite parameters with PureScript"

In this post, I wrote about how we can parse a type-level string to extract parameters that make up a record of parameters we need for our query, where the Symbol can be simply reflected to provide to sqlite3:

<https://qiita.com/kimagure/items/4b08e9f0479d5866ec04>

Tl;dr:

```hs
-- the type signature here is inferred:
getEm
  :: forall a b. J.AllowedParamType a => J.AllowedParamType b
  => SL.DBConnection -> { "$name" :: a, "$count" :: b } -> Aff Foreign
getEm db = J.queryDB db queryP
  where
    queryP = SProxy :: SProxy "select name, count from mytable where name = $name and count = $count"
```

Since I wrote this post, I've given a couple of talks about this library and how people might get to using it.

Long form presented in Milan: <https://speakerdeck.com/justinwoo/superior-string-spaghetti-with-purescript-jajanmen>

Short form presented in HaskellX as a lightning talk: <https://speakerdeck.com/justinwoo/type-classes-pattern-matching-for-types>

There's even a video of the HaskellX talk: <https://skillsmatter.com/skillscasts/13012-lightning-talk-superior-string-spaghetti-with-purescript>
