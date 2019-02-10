---
title: OhYes, you can interop with TypeScript using PureScript
tags: purescript TypeScript
author: kimagure
slide: false
---
Recently, I updated my [OhYes](https://github.com/justinwoo/ohyes-demo) library to the latest [Variant](https://github.com/natefaubion/purescript-variant), which then let me interop with union types of interfaces with a static discriminant field. In this post, I'll go over my demo and how the library is implemented.

Just a warning up front: this post is largely just code examples.

## Demo

### Writing some PureScript

Here, I write some pretty normal code and bundle up values I want to work with into a `utils` record.

```hs
newtype Fruit = Fruit String
derive newtype instance eqFruit :: Eq Fruit
derive newtype instance hasTSRepFruit :: HasTSRep Fruit -- uses the underlying String representation

type State =
  { fruits :: Array Fruit
  }

type Utils =
  { processAction :: State -> Action -> State
  , initialState :: State
  }

utils :: Utils
utils =
  { processAction
  , initialState
  }
  where
    processAction :: State -> Action -> State
    processAction state = match
      { addFruit: \value -> state { fruits = snoc state.fruits value.fruit }
      , removeFruit: \value -> state { fruits = filter ((/=) value.fruit) state.fruits }
      }
    initialState = { fruits: [] }


type Action = Variant
  ( addFruit :: { fruit :: Fruit }
  , removeFruit :: { fruit :: Fruit }
  )
```

The only fancy thing here is that I use a Variant defined with certain keys and types. More on this later.

I then built this and put it in my src/ folder by the following:

`pulp build -O --to src/main.js --skip-entry-point --main Main && echo module.exports = PS.Main >> src/main.js`

Which translates out to "build an optimized bundle to src/main.js, skipping the 'main' entry point, with the main module being 'Main', and then attach the Common.JS export crap to the file".

### Codegen

I write out what name I want to use for the types and run my output through prettier.

```hs
main = launchAff_ do
  writeTextFile UTF8 "./src/generated.ts" values
  where
    values = format defaultOptions $ intercalate "\n"
      [ generateTS "State" (Proxy :: Proxy State)
      , generateTS "Utils" (Proxy :: Proxy Utils)
      , generateTS "Action" (Proxy :: Proxy Action)
      ]
```

I then ran this through `pulp run -m GenerateTypes`, which gives me this output:

```ts
export type State = { fruits: string[] };
export type Utils = {
  initialState: { fruits: string[] },
  processAction: (a: { fruits: string[] }) => (
    a:
      | { type: "addFruit", value: { fruit: string } }
      | { type: "removeFruit", value: { fruit: string } }
  ) => { fruits: string[] }
};
export type Action =
  | { type: "addFruit", value: { fruit: string } }
  | { type: "removeFruit", value: { fruit: string } };
```

And while I could have used newtypes with an instance to use the alias name instead, in this case I chose not to. But most importantly, you can see that my Action definition before is now a union of the two interfaces, one with a string literal field "addFruit" and the other with a string literal field "removeFruit". These could be used from TypeScript directly using type guards too.

### Using from TypeScript

After all this, I can now use this from TypeScript:

```ts
import {State, Utils, Action} from './generated';
import * as Main from './main'

const utils = Main.utils as Utils

const initialState = utils.initialState
const nextState1 = utils.processAction(
  initialState
)({
  type: "addFruit",
  value: {
    fruit: "Apple"
  }
})
const nextState2 = utils.processAction(
  nextState1
)({
  type: "addFruit",
  value: {
    fruit: "Kiwi"
  }
})
const nextState3 = utils.processAction(
  nextState2
)({
  type: "removeFruit",
  value: {
    fruit: "Kiwi"
  }
})

console.log('initialState', initialState)
console.log('  nextState1', nextState1)
console.log('  nextState2', nextState2)
console.log('  nextState3', nextState3)
```

The casting of Main.utils to Utils comes from me not bothering with generating a definitions file or declaration or anything. Other than that though, everything is well-typed here, and after running the TypeScript compiler and running the output, I get the expected:

```
initialState { fruits: [] }
  nextState1 { fruits: [ 'Apple' ] }
  nextState2 { fruits: [ 'Apple', 'Kiwi' ] }
  nextState3 { fruits: [ 'Apple' ] }
```

So yeah, it works! Of course, my setup is probably not exactly what you want, but really, you can make this do whatever you need.

## Library implementation

Funny enough, the actual library has less total code than this demo does and is based on a very simple idea: does a type have a representation that can be used directly from TypeScript? And so the type class is defined:

```hs
class HasTSRep a where
  toTSRep :: Proxy a -> String
```

Where `toTSRep` gives you the type definition in TypeScript.

Here are some example instances:

```hs
instance numberHasTSRep :: HasTSRep Number where
  toTSRep _ = "number"

instance arrayHasTSRep ::
  ( HasTSRep a
  ) => HasTSRep (Array a) where
  toTSRep _ = toTSRep p <> "[]"
    where
      p = Proxy :: Proxy a
```

A regular PureScript function also has a straightforward instance:

```hs
instance functionHasTSRep ::
  ( HasTSRep a
  , HasTSRep b
  ) => HasTSRep (Function a b) where
  toTSRep _ = "(a: " <> a <> ") => " <> b
    where
      a = toTSRep (Proxy :: Proxy a)
      b = toTSRep (Proxy :: Proxy b)
```

And uncurried functions use the normal purescript-functions types:

```hs
instance fn2HasTSRep ::
  ( HasTSRep a
  , HasTSRep b
  , HasTSRep c
  ) => HasTSRep (Fn2 a b c) where
  toTSRep _ =
      "(a: " <> a <>
      ", b: " <> b <>
      ") => " <> c
    where
      a = toTSRep (Proxy :: Proxy a)
      b = toTSRep (Proxy :: Proxy b)
      c = toTSRep (Proxy :: Proxy c)
```

To convert a record into TypeScript interface, I used RowToList to convert the row type into a type level list and iterate the fields, collecting these as a pairs of key and type:

```hs
instance recordHasTSRep ::
  ( RowToList row rl
  , HasTSRepFields rl
  ) => HasTSRep (Record row) where
  toTSRep _ = "{" <> fields <> "}"
    where
      rlp = RLProxy :: RLProxy rl
      fields = intercalate "," $ toTSRepFields rlp

class HasTSRepFields (rl :: RowList) where
  toTSRepFields :: RLProxy rl -> List String

instance consHasTSRepFields ::
  ( HasTSRepFields tail
  , IsSymbol name
  , HasTSRep ty
  ) => HasTSRepFields (Cons name ty tail) where
  toTSRepFields _ = head : tail
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      typ = Proxy :: Proxy ty
      val = toTSRep typ
      head = key <> ":" <> val
      tailp = RLProxy :: RLProxy tail
      tail = toTSRepFields tailp

instance nilHasTSRepFields :: HasTSRepFields Nil where
  toTSRepFields _ = mempty
```

With virtually the same operations, the Variant instance is defined, but using `|` to delimit each member, with each member of the union consisting of the key being reflected to "type", and the field being extracted to "value".

```hs
instance fakeSumRecordHasTSRep ::
  ( RowToList row rl
  , FakeSumRecordMembers rl
  ) => HasTSRep (Variant row) where
  toTSRep _ = intercalate "|" members
    where
      rlp = RLProxy :: RLProxy rl
      members = toFakeSumRecordMembers rlp

class FakeSumRecordMembers (rl :: RowList) where
  toFakeSumRecordMembers :: RLProxy rl -> List String

instance consFakeSumRecordMembers ::
  ( FakeSumRecordMembers tail
  , IsSymbol name
  , HasTSRep ty
  ) => FakeSumRecordMembers (Cons name ty tail) where
  toFakeSumRecordMembers _ = head : tail
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      typ = Proxy :: Proxy ty
      val = toTSRep typ
      head = "{type:\"" <> key <> "\", value:" <> val <> "}"
      tailp = RLProxy :: RLProxy tail
      tail = toFakeSumRecordMembers tailp

instance nilFakeSumRecordMembers :: FakeSumRecordMembers Nil where
  toFakeSumRecordMembers _ = mempty
```

And that's about the whole thing!

## Conclusion

So there wasn't that much to talk about in this post, since it was just about the demo and the implementation, but hopefully this gives you some ideas on how to use my library or implement your own if need be.

The same ideas in this post are used to implement my [Kancho](https://github.com/justinwoo/purescript-kancho) library for constraining and working with Elm.

## Links

* https://github.com/justinwoo/purescript-ohyes
* https://github.com/justinwoo/purescript-kancho
* More links about RowToList: https://www.reddit.com/r/purescript/comments/6mss5o/new_in_purescript_0116_rowtolist/

