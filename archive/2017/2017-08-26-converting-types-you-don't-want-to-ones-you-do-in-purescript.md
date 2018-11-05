# Converting types you don't want to ones you do in Purescript

## Update

As of PureScript 0.12, we no longer need any of these tricks as Generics-Rep will not derive or use `Record` and `Field`, which is good since we can do everything with records directly using `RowToList`!

