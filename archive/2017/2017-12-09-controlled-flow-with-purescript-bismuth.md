Controlled Flow with PureScript-Bismuth
Recently, I've been trying to think of ways PureScript could be easily integrated into an existing project. Considering that most JS projects are in plain JS but compiled with Babel, I started looking into Flow as a way to provide types for all of my PureScript modules and having minimal tooling troubles when working with typical setups.

To this goal, I put together [Bismuth](https://github.com/justinwoo/purescript-bismuth), a library similar to my [OhYes](https://qiita.com/kimagure/items/4847685d02d4b15a556c) library, but for Flow.

## The library details

The library centers around a single type class and method:

```hs
class HasFlowRep a where
  toFlowRep :: Proxy a -> String
```

The "law" here is that all types having an instance of `HasFlowRep` should be able to be represented using a plain Flow type and have their runtime representation match the type. For example:

```hs
instance numberHasFlowRep :: HasFlowRep Number where
  toFlowRep _ = "number"

instance stringHasFlowRep :: HasFlowRep String where
  toFlowRep _ = "string"

instance booleanHasFlowRep :: HasFlowRep Boolean where
  toFlowRep _ = "boolean"

instance recordHasFlowRep ::
  ( RowToList row rl
  , HasFlowRepFields rl
  ) => HasFlowRep (Record row) where
  toFlowRep _ = "{" <> fields <> "}"
    where
      fields = intercalate "," (toFlowRepFields (RLProxy :: RLProxy rl))
```

This seems easy enough, but this *does* mean that all ADTs are banned from being used for types that need to be used from Flow. And while all product types can be rewritten as records, sum types can't be provided so easily. Or can they?

### Variant

[PureScript-Variant](https://github.com/natefaubion/purescript-variant) is a library for representing polymorphic variants in PureScript, such that you can use the rows to track labels by which you will tag Types and where you can readily union a bunch of variants together and whatnot. I take advantage of the runtime representation of this library, a record of a confined-set string literal in "type" and the associated value in "value", to create an instance for `HasFlowRep`:

```hs
instance fakeSumRecordHasFlowRep ::
  ( RowToList row rl
  , FakeSumRecordMembers rl
  ) => HasFlowRep (Variant row) where
  toFlowRep _ = intercalate "|" members
    where
      rlp = RLProxy :: RLProxy rl
      members = toFakeSumRecordMembers rlp

class FakeSumRecordMembers (rl :: RowList) where
  toFakeSumRecordMembers :: RLProxy rl -> List String
```

In short, with this Variant:

```hs
type VariantTest = Variant
  ( a :: String
  , b :: Number
  , c :: Boolean
  )
```

I product this Flow type:

```js
type VariantTest =
  | { type: "a", value: string }
  | { type: "b", value: number }
  | { type: "c", value: boolean };
```

And I can correctly interop between Flow and PureScript, with a fairly idiomatic way to handle polymorphic variants in both -- Flow users already use this sum type approximation and use Flow's type guarding features to discriminate members of the union here, and PureScript users can easily directly use the Variant or convert from the Variant into some concrete sum types or whatever they may choose to do.

Omitted are the copious use of `RowToList` in the implementation, but those interested may read the source directly or read my OhYes post which describes how the implementations are done.

## The usage details

There are essentially four steps to use this to use this library in a project

1. Introduce some tooling of choice e.g. Flow config, Webpack + Babel loader
2. Write some PureScript functions
3. Write a code generation module that uses either direct type generation or library definition functions and run it
4. Consume your PureScript output in Flow e.g. from output directly with library definitions or from some other output or whatever you want
5. Repeat 2-4 as desired

### Step 1: some setup hell

I chose use Webpack with Babel-loader using the react-native preset (Flow, ES6+, etc.):

* [package.json](https://github.com/justinwoo/bismuth-example/blob/b088e0c2faf714f847f0025013174df1e56239bf/package.json)
* [webpack.config.js](https://github.com/justinwoo/bismuth-example/blob/b088e0c2faf714f847f0025013174df1e56239bf/webpack.config.js)
* [.flowconfig](https://github.com/justinwoo/bismuth-example/blob/b088e0c2faf714f847f0025013174df1e56239bf/.flowconfig)

### Step 2: write some PureScript

I wrote two simple functions and one involved one:

```hs
add2 :: Number -> Number
add2 x = x + 2.0

log :: String -> Eff (console :: CONSOLE) Unit
log s = EffC.log s

-- for my own use, I do not expose this in my type gen
newtype MyError = MyError String
derive newtype instance errorHasFlowRep :: HasFlowRep MyError

-- some validators that I use below
-- [...]

-- the underlying variant type for my validation result
type ValidationResultType = Variant
  ( errors :: Array MyError
  , success :: String
  )

-- the validation result that I expose in my codegen as a type alias
-- that i refer to by name, for convenience
newtype ValidationResult = ValidationResult ValidationResultType
instance validationResultHasFlowRep :: HasFlowRep ValidationResult where
  toFlowRep _ = "ValidationResult" -- name given to this type alias

-- function for validating a string with some rules, producing my variant newtype
validateInput :: String -> ValidationResult
validateInput s =
  let
    result = s <$ startsWithI s <> containsDid s <> endsWithPeriod s
  in
    ValidationResult $ unV -- "runs" the validation
        -- error case, creates { type: "errors", ... }
      (inj (SProxy :: SProxy "errors"))

        -- success case, creates { type: "success", ...}
      (inj (SProxy :: SProxy "success"))

        -- my validation result
      result
```

So while this the Validation part seems involved, I wrote about them [here](seems pretty involved, yo) if you want to find out more.

And so you can see here cases in which I use a newtype that is not exposed in codegen and a newtype that is exposed in codegen for legibility convenience. By this point, you may ask, "wait, aren't all of these types type aliases, since those are the only types that can be guaranteed to work interoperably?" And I would say, yes, that is exactly right, which is why it almost doesn't matter at all whether we expose the names or not, only that we should try to make this legible.

### Step 3: write some code (type) gen

So in the previous section, I decided to declare one type alias explicitly and expose three outputs as typed output. For this, I use some helper functions for declaring modules defined in Bismuth.Lib along with prettier to format the output:

```hs
values = format defaultOptions $ "// @flow\n" <>
  createModuleDefinition
    "../output/Main"
    [ declareFlowType
        (toFlowRep (Proxy :: Proxy ValidationResult)) -- ensure same alias name
        (Proxy :: Proxy ValidationResultType) -- alias body
    ]
    (fromFoldable
      [ Tuple "add2" (toFlowRep' Main.add2)
      , Tuple "log" (toFlowRep' Main.log)
      , Tuple "validateInput" (toFlowRep' Main.validateInput)
      ])
```

And so my `createModuleDefinition` function takes the module name to be used (I chose to import it from output directly), an array of declarations to be added to the definition, and a string map of exports' types.

I then write these out to a file like so:

```hs
main = launchAff_ do
  writeTextFile UTF8 "./flow-lib/GeneratedDef.js" values
```

For which the output is the following:

```js
// @flow
declare module "../output/Main" {
  declare type ValidationResult =
    | { type: "errors", value: string[] }
    | { type: "success", value: string };

  declare module.exports: {
    add2: (a: number) => number,
    log: (a: string) => () => any,
    validateInput: (a: string) => ValidationResult
  };
}
```

And we can see the `ValidationResult` came out correctly, and the `Eff` in log is correctly represented as a thunked function.

### Step 4: consume the output from Flow

Now that this setup has been done, we can import `../output/Main` and have it be well typed. The example speaks for itself:

```js
// @flow
import * as Main from '../output/Main';

function validateAndLog(s: string) {
  const result: Main.ValidationResult = Main.validateInput(s);

  switch (result.type) {
    case "errors":
      // type guarded as the errors :: string[] case
      return Main.log(`got errors from ${s}: ${JSON.stringify(result.value)}`)();
    case "success":
      // type guarded as the success :: string case
      return Main.log(`success: ${result.value}`)();
  }
}


const num = Main.add2(12);
Main.log(num.toString())();
validateAndLog("I did it.");
validateAndLog("I did it");
validateAndLog("");
validateAndLog("did");

// output:
// > node dist/index.js
//
// 14
// success: I did it.
// got errors from I did it: ["didn't end with '.'"]
// got errors from : ["didn't start with 'I'","didn't contain 'did'","didn't end with '.'"]
// got errors from did: ["didn't start with 'I'","didn't end with '.'"]

// correctly errors:
// Main.add2('sdf');
// Main.log(123)();
// Main.validateInput(123);
```

### Running this all

I have a bunch of npm scripts tasks set up to do this, but they are equivalent to these commands:

```
$ pulp build # build my project's purescript sources
$ pulp run --main GenerateTypes # run GenerateTypes
$ webpack --progress --colors # build my project with webpack
$ killall flow; flow # run flow, making sure that the flow session is fresh
$ node dist/index.js # run our flow output for good measure
```

And that's it!

## Conclusion

Hopefully this has shown you that interoperating with JS doesn't have to be painful or overly error-prone by using Flowtype integration. Please check out the example to see the whole thing in action and suggest any changes you'd like to see!

## Links

* Example repo: https://github.com/justinwoo/bismuth-example
* The library: https://github.com/justinwoo/purescript-bismuth
