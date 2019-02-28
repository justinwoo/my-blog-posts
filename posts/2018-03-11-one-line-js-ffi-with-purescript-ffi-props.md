---
title: One-line JS FFI with PureScript-FFI-Props
tags: purescript
author: kimagure
slide: false
---
While I don't often write FFI, there are often times when I work with modules that are sensible enough that I don't have to deal with dynamic types. In these cases, I don't really need to write so much of the FFI in the JS side and only need to work with well-typed interfaces in PureScript.

To do this, I made a library that allows for declaring that a foreign data type has some members of some rough representational types, which then I can wrap with more sensible PureScript functions and signatures.

## FFI-Props

The core of this library lies in the newtype `Object`:

```hs
newtype Object object (properties :: # Type) = Object object
```

where `object` is where one puts their own foreign data type and `properties` is a row type of the properties that object has. Then I have get, set, and modify functions of the form

```hs
unsafeGetProp
  :: forall o p p' name ty e
   . IsSymbol name
  => RowCons name ty p' p
  => SProxy name
  -> Object o p
  -> Eff e ty
unsafeGetProp _ = EU.runEffFn2 _unsafeGetProp name
  where
    name = reflectSymbol (SProxy :: SProxy name)
```

The constraints in this function check that whichever property of `name` is trying to be retrieved is in the properties row, and returns the value in the effect with the type `ty`. *If you're unfamiliar with the RowCons type class, you might read through my slides about Simple-JSON and generic Record operations [here](https://speakerdeck.com/justinwoo/easy-json-deserialization-with-simple-json-and-record)*

## Usage

As an example, I made a demo repository showing how to use [`he`](https://www.npmjs.com/package/he) module. In this repo, I have a single line in `src/Main.js`:

```js
exports.he = require("he");
```

With this, I'm ready to get to work. First, I declare a foreign data type for the module:

```hs
foreign import data HE :: Type
```

Then I use the `Object` type from my library to wrap this in a newtype for the foreign import value:

```hs
import Data.Function.Uncurried as FU
import FFIProps as FP

foreign import data HE :: Type
foreign import he
  :: FP.Object
       HE
       ( version :: String
       , encode :: FU.Fn2 String Foreign String
       , decode :: FU.Fn2 String Foreign String
       )
```

So you can see here that I have declared that `he` is an `Object` type where the underlying type I use is `HE`, with the properties `version`, `encode`, and `decode` with those signatures.

And so to get the `version` property, I define a function using the `unsafeGet` from above, and the constraints can be solved to give me the concrete signature:

```hs
getVersion :: forall e. Eff e String
getVersion = FP.unsafeGetProp (SProxy :: SProxy "version") he
```

And I'm content to leave this in Eff in that reading the property from the foreign object is an effectful operation and it could be changed at any time anyhow.

In the case of encoding the strings, I first started to define a newtype for encoded strings:

```hs
newtype EncodedString = EncodedString String
derive instance newtypeES :: Newtype EncodedString _
```

Then I decided to define a placeholder type for all the properties possible in the encoding options, which I could fill in later:

```hs
type EncodeOptions =
  (
  )
```

Then I first wrote the type signature I wanted to work with, where I could pass in a record that contained a subset of the fields defined in EncodeOptions.

```hs
encodeString
  :: forall options options'
   . Union options options' EncodeOptions
  => Record options
  -> String
  -> EncodedString
```

Since this method would not be mutated, I chose to "unsafely" perform the operation to get the `encode` property, which was `FU.Fn2 String Foreign String`. Then all was left was to convert my options record into the opaque `Foreign` type and run the inlined function:

```hs
encodeString options s
  = EncodedString
  $ FU.runFn2 encode s (toForeign options)
  where
    encode
      = unsafePerformEff
      $ FP.unsafeGetProp (SProxy :: SProxy "encode") he
```

And the same done for decode.

## Result

With my bindings completed, I could put them to use and get the results I expected out smoothly:

```hs
main = do
  version <- getVersion
  log $ "HE is version " <> version
  let
    encoded = encodeString {} "foo © bar ≠ baz 𝌆 qux"
    decoded = decodeString {} (wrap "foo &copy; bar &ne; baz &#x1D306; qux")
  log $ "encoded " <> unwrap encoded
  log $ "decoded " <> decoded

  -- HE is version 1.1.1
  -- encoded foo &#xA9; bar &#x2260; baz &#x1D306; qux
  -- decoded foo © bar ≠ baz 𝌆 qux
```

Voila.

## Conclusion

Hopefully this has shown you how you can use this library too if you wish, and how to use newtypes to store extra information you can use elsewhere.

## Links

* This repo: https://github.com/justinwoo/ffi-props-demo-he
* FFI Props: https://github.com/justinwoo/purescript-ffi-props

