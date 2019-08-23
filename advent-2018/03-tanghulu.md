While most people associate PureScript with row types, capabilites of Symbols has grown with each release of PureScript, to the point that now we have a Symbol Cons type class now in PureScript 0.12.x to split a known symbol to its head and tail. These Symbol type classes let us apply dependently-typed techniques using statically defined type-level strings.

## "We don't need Peano Numbers in PureScript"

Of course, any demonstration of dependently-typed techniques would not be complete without some obnoxious sized list demo, so I wrote about one using the type class `Prim.Symbol.Append`:

<https://qiita.com/kimagure/items/522fa4dd4abdcc313c8e>

The gist of this post is that when we look at the definition of the type class and its functional dependencies, we can treat this like addition of two numbers to a sum:

```hs
class Append (left :: Symbol) (right :: Symbol) (appended :: Symbol)
  | left right -> appended
  , right appended -> left
  , appended left -> right
```

That is,

```
a <> b == c
  | a b -> c
  , b c -> a
  , a c -> b
```

And so with `"." <> ".." == "..."` and the fundeps above, we can get to work making arithmetic work for symbols of period `"."`. Amazing!

We'll get more into how to use Symbol for more fun things in future posts.
