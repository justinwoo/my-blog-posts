While the current set of compiler type classes are pretty nice, sometimes there's not a class for something I'd like to do. Other times, there's an operation I could perform manually with the existing classes, but the inference and performance of solving these constraints is terrible.

Whatever the reason, it would be nice to define some of my own compiler type classes.

## "Implement your own compiler type class in PureScript"

In this post, I go over what all is involved in lining up the pieces in the PureScript compiler codebase to add your own compiler-solved type class.

<https://qiita.com/kimagure/items/8736fe6a2f25da526368>

Once we have all of the pieces in place from a few dozen lines of adding new definitions, documentation, and functional dependencies information, we can write the core logic that solves our instances. For example, checking if a Symbol pattern is contained within another Symbol and getting a Boolean-kinded result:

```hs
containsSymbol :: Type -> Type -> Type -> Maybe (Type, Type, Type)
containsSymbol patt@(TypeLevelString patt') sym@(TypeLevelString sym') _ = do
  flag <- T.isInfixOf <$> decodeString patt' <*> decodeString sym'
  pure (patt, sym, TypeConstructor $ if flag then C.booleanTrue else C.booleanFalse)
containsSymbol _ _ _ = Nothing
```
