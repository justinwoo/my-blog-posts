---
title: Implement your own compiler type class in PureScript
tags: purescript
author: kimagure
slide: false
---
In PureScript, you might have seen that some type classes are implemented in the compiler's `Prim` modules, for various reasons such as better type inference, using information not available in the type system, efficiency of generated instances, avoiding redundant instances, etc. There may be many times when you also need to implement your own compiler type class for solving things not possible in PureScript currently, such as was the case with `Row.Cons`, `RowToList`, `Symbol.Cons`, and more.

In this post, we'll look at what it looks like to add a compiler type class with the current state of the PureScript compiler. For our example, we'll add a type class that can tell us if a `Symbol` contains a `Symbol` and returns a result of `kind` `Boolean`.

## Adding a pattern synonym

First, all of the existing compiler type classes have a pattern associated with them, so we will want to add this to `src/Language/PureScript/Constants.hs` with the proper `Prim` submodule.

```hs
pattern SymbolContains :: Qualified (ProperName 'ClassName)
pattern SymbolContains = Qualified (Just PrimSymbol) (ProperName "Contains")
```

## Adding type and class entries

We'll want to add an entry to the appropriate submodule types list, so first we add to `primSymbolTypes`:

```hs
    , ( primSubName C.moduleSymbol "Contains"
      , ( kindSymbol -:> kindSymbol -:> kindBoolean -:> kindConstraint
        , ExternData
        )
      )
```

So we see here that we have declared here like the other types that we use the name "Contains", and it has a kind signature of `Symbol -> Symbol -> Boolean -> Constraint`, meaning that a given constraint using `Contains` will have these parameters.

Then we add this to the appropriate submodule classes map our type class data, for what the parameters should be referred to as and the functional dependencies involved in `primSymbolClasses`:

```hs
    -- class Contains (pattern :: Symbol) (symbol :: Symbol) (result :: Boolean)
    -- | pattern symbol -> result
    , (primSubName C.moduleSymbol "Contains", makeTypeClassData
        [ ("pattern", Just kindSymbol)
        , ("symbol", Just kindSymbol)
        , ("result", Just kindBoolean)
        ] [] []
        [ FunctionalDependency [0, 1] [2]
        ])
```

So here we have declared that the functional dependency of this class is that the pattern and the symbol determine the result, since we can use the pattern with the symbol to solve if the pattern is contained in the symbol.

## Adding to entailment

To add solving for our type class, we should add it to `src/Language/PureScript/TypeChecker/Entailment.hs` as a matched definition of `forClassName`:

```hs
    forClassName _ C.SymbolContains args | Just dicts <- solveSymbolContains args = dicts
```

And now we can define `solveSymbolContains` like how many other functions for solving instances are defined. Like many, we split up the destructuring and handling of the returned type class dictionary type:

```hs
    solveSymbolContains :: [Type] -> Maybe [TypeClassDict]
    solveSymbolContains [arg0, arg1, arg2] = do
      (arg0', arg1', arg2') <- containsSymbol arg0 arg1 arg2
      let args' = [arg0', arg1', arg2']
      pure
        [TypeClassDictionaryInScope [] 0 EmptyClassInstance []
           C.SymbolContains args' Nothing]
    solveSymbolContains _ = Nothing
```

As this is a class with only a type class instance, we end up using `EmptyClassInstance` like in other compiler solved classes.

Then our actual solver ends up being a matter of working with the `Symbol` representation of the compiler and applying a text function to figure out if we should return true or false:

```hs
    containsSymbol :: Type -> Type -> Type -> Maybe (Type, Type, Type)
    containsSymbol patt@(TypeLevelString patt') sym@(TypeLevelString sym') _ = do
      flag <- T.isInfixOf <$> decodeString patt' <*> decodeString sym'
      pure (patt, sym, TypeConstructor $ if flag then C.booleanTrue else C.booleanFalse)
    containsSymbol _ _ _ = Nothing
```

So at the end of the day, we take either the `booleanTrue` or `booleanFalse` constant types defined in Constants module to fulfill the `Boolean` kind argument. And this is about it, where the core logic of this compiler type class has been implemented in roughly a dozen lines of code.

## Wrapping up and adding to documentation

To have documentation for our class, we should add an entry for it in our docs for `Prim` modules in `src/Language/PureScript/Docs/Prim.hs`.

First, we should add an entry to the `Prim` submodule. In our case, this will go to the `primSymbolDocsModule`:

```diff
  , modDeclarations =
      [ symbolAppend
      , symbolCompare
      , symbolCons
+     , symbolContains
      , symbolBreakOn
      ]
```

Then we'll define a `Declaration` below:

```hs
symbolContains :: Declaration
symbolContains = primClassOf (P.primSubName "Symbol") "Contains" $ T.unlines
  [ "Compiler solved type class for checking if a Symbol contains a Symbol."
  ]
```

And that's all we need for our docs. If you build the compiler at this stage and run `pulp docs -- --format html` and open `generated-docs/Prim.Symbol.html`, you will see the entry in the docs:

![](https://i.imgur.com/WtAbAz4.png)

## Usage

We should add a simple usage to our passing tests to make sure this works in the cases we want. To that purpose, we can add a test file to `tests/purs/passing` called `SymbolContains.purs`:

```hs
module Main where

import Effect.Console

import Data.Symbol (SProxy(..))
import Prim.Boolean as Boolean
import Prim.Symbol as Symbol
import Type.Data.Boolean (BProxy(..))

symbolContains
  :: forall pattern sym result
   . Symbol.Contains pattern sym result
  => SProxy pattern
  -> SProxy sym
  -> BProxy result
symbolContains _ _ = BProxy

-- inferred type:
resultContains1 :: BProxy Boolean.True
resultContains1 = symbolContains (SProxy :: SProxy "b") (SProxy :: SProxy "abc")

-- inferred type:
resultContains2 :: BProxy Boolean.False
resultContains2 = symbolContains (SProxy :: SProxy "z") (SProxy :: SProxy "abc")

main = log "Done"
```

In reality, I created this test file after building the compiler, so I used the IDE tools to help me generate those type signatures here. So in the test cases, the first tests that "abc" contains "b" and the second that "abc" does not contain "z". Nice!

## Conclusion

Hopefully this has shown you that you can readily add some of your own compiler type classes to PureScript with minimal changes. If you have some use cases currently that suffer from some limitations in some way and you don't want to resort to error-prone code generation, you might consider forking the PureScript compiler to add a compiler type class that you might need, and maybe upstream those changes if they work out.

In my case, I have a bit of a need for this class along with the ability to break a `Symbol` into its head and tail components so I can more correctly work with some type-level string parsing, but sky's the limit for what kind of things you need to do.

## Links

* PR for `Symbol.BreakOn` https://github.com/purescript/purescript/pull/3383

