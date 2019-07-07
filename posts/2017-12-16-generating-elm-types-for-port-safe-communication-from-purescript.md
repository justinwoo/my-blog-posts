# Generating Elm Types for Port-Safe Communication from PureScript

# Generating Elm Types for Port-Safe Communication from PureScript

A while ago, I wrote about how I integrated an Elm app into a PureScript one [here](https://qiita.com/kimagure/items/d12525d42516f95dd541). That approach used Generics-Rep to extract information from the types and write codegen from it, but had some deficiencies that made it not so ideal. Thankfully, RowToList came out soon after I originally wrote the library, and that allowed me to work with type information first-class that I couldn't get access to before -- namely, records.

This post will go more into the implementation part of the library and try to keep it simple enough so that users who would like to use this library can understand more about how things work, and so that those who would like to develop their own similar solutions can take some inspirations. The demo using this library is available [here](https://github.com/justinwoo/purescript-halogen-elm-etch-sketch).

## What is Kancho?

Kancho is a library that I wrote to constrain PureScript types so that data could be sent directly through Elm ports without conversion to and from some other compatible format like JSON. This allows me to:

* Not have to write/generate Decoder code
* Get access to Elm's automatic JS port de/encoding
* Get pretty good performance of ports data as a result (or as fast as any normal Elm JS ports code)

*You may ask, "why can't I use the automatic JS port de/encoding in Elm through magic type variables like how `comparable` works?", and I will say that I've asked about it for years, but the end conclusion is that it's preferable to have codegen approaches over exposing this "low-priority" feature to users. Don't blame me.*

### HasElmPortVersion

The main mechanism for how this library is in one type class:

```hs
class HasElmPortVersion ty where
  toElmTypeRep :: Proxy ty -> String
```

So that all types having an instance of this type class have some way that they can be represented in Elm as a type and they are marked as safe to use through ports without conversion. You might even call these "laws".

And so, I can write some functions with this constraint that are generally useful:

```hs
toElmModel :: forall a. HasElmPortVersion a => a -> a
toElmModel = id

getElmRep :: forall a
   . HasElmPortVersion a
  => Proxy a
  -> String
getElmRep _ = toElmTypeRep (Proxy :: Proxy a)
```

The first function lets me guarantee that the constraint has been applied, so while it is a no-op, I can use this to make sure that the type used in a codepath has an instance, and thus, has an Elm port compatible runtime representation.

The second function wraps the type class method to be more obvious how it should be used. It's important to note that since we have applied the `HasElmPortVersion` constraint on this type variable `a`, we then get access to the method `toElmTypeRep` for `a`. So while a completely unconstrained type variable cannot have any operations performed on it to work for all types, by limiting the valid types to those that satisfy this constraint that they have an instance of our type class, we are able to perform operations with methods of our type class on this type.

### Instances of HasElmPortVersion

There are some pieces of documentation available about what types are able to be used in ports, like [here](https://guide.elm-lang.org/interop/javascript.html#customs-and-border-protection). Using this information, we can write some of our instances.

```hs
instance hasElmPortVersionInt :: HasElmPortVersion Int where
  toElmTypeRep _ = "Int"

instance hasElmPortVersionNumber :: HasElmPortVersion Number where
  toElmTypeRep _ = "Float"

instance hasElmPortVersionString :: HasElmPortVersion String where
  toElmTypeRep _ = "String"

instance hasElmPortVersionBoolean :: HasElmPortVersion Boolean where
  toElmTypeRep _ = "Bool"
```

And as Arrays can be parsed into List and worked with more easily, I can define the instance with the constraint that the type inside the Array is representable:

```hs
instance hasElmPortVersionArray ::
  ( HasElmPortVersion inner
  ) => HasElmPortVersion (Array inner) where
  toElmTypeRep _ = "List " <> toElmTypeRep (Proxy :: Proxy inner)
```

### The Record instance

So records are a hairy thing that in Haskell and pre-RowToList PureScript you'd have to go out and reach for GHC Generics/Generics-Rep for, as you need to be able to iterate the fields of the record to get information you need. And so was done the last version, but now with RowToList we don't have to worry about the restriction.

Remember that in PureScript, the record type syntax is just syntactic sugar:

```hs
-- this is a normal record type declaration
type MyRecord = { a :: String, b :: Int }
-- this is what it actually is in the end
type MyRecord = Record ( a :: String, b :: Int )
-- so Record takes a row of types (# Type) and returns a concrete type (Type) 
```

A row type is an unordered collection of fields of labels (Symbol) and their types (Type), but you can't iterate an unordered collection without some kind of ordering mechanism. And so, RowToList provides us this utility to take that row information and turn it into a type-level list RowList:

```hs
RowToList
  ( a :: String, b :: Int )
  (Cons "a" String (Cons "b" Int Nil))
```

This typeclass is solved by the compiler for a given row type, so we can use this information to convert any row type into a RowList and write instances for them.

So we can write our Record instance of `HasElmPortVersion` using this and another type class that we define:

```hs
instance hasElmPortVersionRecord ::
  ( RowToList fields fieldList
  , HasElmPortVersionFields fieldList
  ) => HasElmPortVersion (Record fields) where
  toElmTypeRep _ =
    "{" <> contents <> "}"
    where
      contents = intercalate "\n  , " $
        extractFields (RLProxy :: RLProxy fieldList)

class HasElmPortVersionFields (xs :: RowList) where
  extractFields :: RLProxy xs -> List String
```

### Extracting Fields

Our `HasElmPortVersionFields` works by matching the two cases of any normal list: the nil and the cons case.

In the nil case, we know that we will have no more fields to iterate through and so we will have no more fields to convert, so we want to just return an empty list.

```hs
instance hasElmPortVersionAndFieldsNil :: HasElmPortVersionFields Nil where
  extractFields _ = mempty
```

In the cons case, we will have our current field to convert and the rest of the fields to return. In this case we can extract the field as `label : Type` by reflecting the Symbol to a string and getting the Elm type representation of the type.

```hs
instance hasElmPortVersionAndFieldsCons ::
  ( IsSymbol name
  , HasElmPortVersion ty
  , HasElmPortVersionFields tail
  ) => HasElmPortVersionFields (Cons name ty tail) where
  extractFields _ = field : rest
    where
      name = reflectSymbol (SProxy :: SProxy name)
      tyName = toElmTypeRep (Proxy :: Proxy ty)
      field = name <> " : " <> tyName
      rest = extractFields (RLProxy :: RLProxy tail)
```

And that's all!

### Usage

So with this, we are now able to generate the Elm type signatures for types that will go through Elm ports without any conversion needed. Let's try a simple example use case.

Say we have a Coords newtype, meaning that in runtime, it'll be the same as its underlying structure and the information is used mostly for our types. We'll then create a type alias EtchSketch that will contain Coords and an Array coords as properties.

However, to make sure that when Coords is used in another type that we'll only have "Coords" used instead of the type inlined, we'll need to make sure that the newtype's instance for `HasElmPortVersion` is defined to only output the name.

```hs
newtype Coords = Coords
  { x :: Int
  , y :: Int
  }
-- convenience Newtype instance used later
derive instance newtypeCoords :: Newtype Coords _
-- custom HasElmPortVersion instance to only output the name
instance hasElmPortVersionCoords :: HasElmPortVersion Coords where
  toElmTypeRep _ = "Coords"

type EtchSketch =
  { cursor :: Coords
  , points :: Array Coords
  }
```

In the case of `EtchSketch`, we only need to call `getElmRep` with a Proxy:

```hs
etchSketchType =
  "type alias EtchSketch = " <>
  getElmRep (Proxy :: Proxy EtchSketch) 
```

But for the Coords, to get the newtype's inner type rep, we need to write a little helper:

```hs
newtypeInnerProxy :: forall a rec
    . Newtype a rec
  => HasElmPortVersion rec
  => Proxy a
  -> Proxy rec
newtypeInnerProxy _ = Proxy
```

So this function uses our `Newtype` instance from before to extract the record type from inside the newtype and returns a Proxy of it so that we can use it in other functions. And so getting the coords type becomes a matter of calling this function.

```hs
coordsType =
  "type alias Coords = " <>
  getElmRep (newtypeInnerProxy $ Proxy :: Proxy Coords)
```

And when we run these as part of a big codegen definition, we get these results inside:

```hs
type alias Coords =
    { x : Int
    , y : Int
    }

type alias EtchSketch =
    { cursor : Coords
    , points : List Coords
    }
```

And that's what we need in the end!

## Conclusion

I hope this has shown you how the Kancho library works and how you might use the first-class type information available through row types to write anything you need for records and for anything where you need to constrain types. I have some similar projects like [OhYes](https://github.com/justinwoo/purescript-ohyes) for TypeScript and [Bismuth](https://github.com/justinwoo/purescript-bismuth) for Flow, and I think the most useful applications of this kind of thing might actually not even be for codegen for other programming language but maybe for communication with certain other libraries, communication of serializable data structures, and more.

Hopefully this was useful to read, please let me know if you have any questions about this library or this approach in general on [Twitter@jusrin00](https://twitter.com/jusrin00). Thanks for reading!

## Links

* Kancho https://github.com/justinwoo/purescript-kancho
* Demo using Kancho https://github.com/justinwoo/purescript-halogen-elm-etch-sketch
* More things about RowLists https://github.com/justinwoo/awesome-rowlist