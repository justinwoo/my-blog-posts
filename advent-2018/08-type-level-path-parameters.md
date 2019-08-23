Typically, approaches to represent parameterized paths in the type level have required making some kind of type-level operator of kind `Type -> Type -> Type`, so that you could define routes in a manner such as `type MyRoute = Literal "hello" / Param "name"`. While this seems "explicit", it also seems at least a little bit silly: why can't we use Symbols to define these routes?

Csongor wrote about how Symbol.Cons was implemented for PureScript, with an example library for deriving variadic functions here: <http://kcsongor.github.io/purescript-safe-printf/>. He also then made a record-based formatting library here: <https://github.com/kcsongor/purescript-record-format>. Surely we can take advantage of these to make a type-level path solution?

## "Well-typed path params in PureScript 0.12"

In this post, I talked about how we can use the type-level parsing result from record-format to make a path:

<https://qiita.com/kimagure/items/3273d20c4c5ad74dbe26>

Knowing that we are going to parse URLs, we can use `/` as delimiters, where we can match literal and variable sections. In building up the variable results, we can use [Record.Builder](https://pursuit.purescript.org/packages/purescript-record/1.0.0/docs/Record.Builder) to build up a record of the parameters we have parsed into a record of strings, where the key is the name that we gave our parameter in this scheme:

```hs
url :: SProxy "/hello/{name}/{age}"
```

In the future, we'll talk about how we can add optional type annotations to this Symbol, so that we don't need to work with a record of strings being converted by key to a proper heterogeneous record.
