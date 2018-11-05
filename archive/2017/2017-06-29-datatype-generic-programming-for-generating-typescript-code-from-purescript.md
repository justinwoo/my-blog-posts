# Datatype-Generic programming for generating TypeScript code from Purescript

*A guide I could have used getting started*

By no means is this going to be a comprehensive review, or really even present anything new to the topic, and I'll try to add links to much better, in-depth explanations below the article. I hope this will help people like me, who've never really tried to do any Generic programming (no, not the "Java/OOP Generics") get started in doing something for fun.

## What

Datatype-Generic programming, most commonly known as simply "Generic programming" to FP people, is a fun way to program using representations of types. By working with the representation of your types, you can write code that will operate on these representations that will then be applicable to all code that has a type representation.

I *would* talk about how applications of Generic programming for everything from comparisons to JSON de/serialization to Lens and whatever are amazingly useful and all, but I'm going to talk about my demo instead, where I generated some Typescript using Purescript's Generics-Rep.

## The Problem

I have a Node backend written in Purescript that can take and return JSON, so I've modeled it with a phantom type:

```hs
data Method
  = GET
  | POST
derive instance genericMethod :: Generic Method _

newtype Route req res = Route
  { method :: Method
  , url :: String
  }
derive instance genericRoute :: Generic (Route req res) _
```

So for a given Route, there are request and response types for the payloads, and then the actual method that will be used and the URL. Using this, I've written these data types and route definitions:

```hs
newtype Unused = Unused String
derive instance genericUnused :: Generic Unused _

newtype Flag = Flag Boolean
derive instance genericFlag :: Generic Flag _

newtype Path = Path String
derive instance genericPath :: Generic Path _

files :: Route Unused (Array Path)
files = Route
  { method: GET
  , url: "/api/files"
  }

watched :: Route Unused (Array FileData)
watched = Route
  { method: GET
  , url: "/api/watched"
  }
```

In this case, I have an idea of the rough *concrete* thing I want to work with in Typescript, where each Route definition will mimic what I have here. I'll reach for the handy [Purescript Generics-Rep](https://pursuit.purescript.org/packages/purescript-generics-rep/5.1.0) library to start working, with the Purescript compiler providing [Generic instance derivation](https://github.com/purescript/documentation/blob/master/guides/Generic.md).

### Writing out Routes

Some considerations:

1. For every other type created with a constructor, I want its name to be returned quickly.
2. Primitives don't have generic instances, so their representation ends up being the actual types themselves.
3. I want to actually unwrap arrays, since "Array" by itself isn't useful

For extracting names from reps, I'm going to define a "generic type name" type class like so:

```hs
class GTypeName rep where
  gTypeName :: forall f. f rep -> String
```

The reason I use `f` here is because I want to support any type like `Proxy` here, since I'm not going to be using `from` to get type reps from their values.

#### A few rep data types

There are a couple of rep data types that we'll need to know about  before starting to extract the TypeScript-style type names from our route.

* `newtype Constructor (name :: Symbol) a`

A Constructor is a data type representing a data constructor. It's defined with a Symbol name and an argument type (A Symbol is a type-level string that we'll get at when extracting names).

* `newtype Argument a`

An Argument gets supplied to other reps and comes with its own paramter of possible types.

#### Extracting type names

Let's start with our Constructor instance:

```hs
instance gTypeNameConstructor ::
  ( IsSymbol f
  ) => GTypeName (Constructor f a) where
  gTypeName _ = reflectSymbol (SProxy :: SProxy f)
```

The IsSymbol constraint is used here so that we can use its method reflectSymbol on string proxies (SProxy) of `f`.

Then our Argument instance:

```hs
instance gTypeNameArgument ::
  ( Generic f rep
  , GTypeName rep
  )
  => GTypeName (Argument f) where
  gTypeName _ = gTypeName (Proxy :: Proxy rep)
```

Here, we introduce `f` has a Generic instance where the type representation will be named `rep`, which we will then put a constraint on by saying that it must have a instance for GTypeName. Our term passes along the rep to another gTypeName call.

In our data types, we have two primitives that we're using - `String` and `Boolean`. In Typescript land, they call these `string` and `boolean`, so we should have appropriate instances for each.

```hs
instance gTypeNameString :: GTypeName String where
  gTypeName _ = "string"

instance gTypeNameBoolean :: GTypeName Boolean where
  gTypeName _ = "boolean"
```

Now the last thing we need to handle: Arrays. Arrays themselves are data constructors, so if we don't write an instance for them, we will get back "Array". Well, we can purposefully write an overlapping instance.

```hs
instance gTypeNameArray ::
  ( GTypeName a
  ) => GTypeName (Constructor "Array" a) where
  gTypeName _ = gTypeName (Proxy :: Proxy a) <> "[]"
```

So here, you can see that I provide the name with a symbol literal, and then I dig down into the contents of the array for its typename, which a [] is appended to. as a true TypeScript-style array. *Note that this overlapping instances trickery is normally not needed for any sane application of Generics, but we may get various ways to route these instances in a more disciplined manner in Purescript sometime.*

With these instances, we're ready to get to work. We can define a simpler function to call to not get lost in the gory details:

```hs
nameOf :: forall a rep
  . Generic a rep
  => GTypeName rep
  => Proxy a
  -> String
nameOf _ = gTypeName (Proxy :: Proxy rep)
```

This way, you can just pass a Proxy of your type in, and this method will translate the call to one using reps for you.

#### Writing the Route definition

With extracting types taken care of, we can now write some pretty simple code for generating our definitions:

```hs
writeRouteDefinition :: forall req res a b
  . Generic req a
  => Generic res b
  => GTypeName a
  => GTypeName b
  => String
  -> Route req res
  -> String
writeRouteDefinition name route@Route{method, url} =
  "const " <> name
    <> ": Route<" <> reqName <> ", " <> resName <>  "> = {\n"
    <> "  method: \"" <> method' <> "\",\n"
    <> "  url: \"" <> url <> "\"\n"
    <> "}\n"
  where
    reqName = nameOf (Proxy :: Proxy req)
    resName = nameOf (Proxy :: Proxy res)
    method' = show method
```

Like before, we add constraints to create variables for the actual reps and add instances for GTypeName for these. Our actual arguments consist of the name we want to give the route and a route definition, returning a string to print out.

When we run this with the files route from above, we'll get the following output:

```ts
const files: Route<Unused, Path[]> = {
  method: "GET",
  url: "/api/files"
}
```

This is amazing, right??

### Writing out Types

Okay, so this part is much more involved, since we need to actually go through almost the whole structure. But surprisingly, there's not really too much more involved here given a few conditions:

1. I only deal with newtypes of primitives and newtypes of records. These are the most you can sanely represent in JSON. But that's fine I think.
2. I'm not going to currently bother with real sum or product types. We run into different methods and ideas about encoding these in Generic JSON libraries, but let's not bother with this for now.
3. I will use GTypeName to bail out early like I did before, since I just need identifiers here. Normally, we could just write totally normal code for traversing the whole structure, but here we are.

#### A few more rep data types

* `newtype Rec fields`

In Purescript, we have first-class Record data constructors, then take some fields which are normally defined by row types. In the sense of Generics though, those rows are just elements of product types.

* `newtype Field (field :: Symbol) a`

This is the Field of a record, which consists of a symbol for the property name and a type for its value.

* `data Product a b`

Products represent the constructors with multiple fields, which in the case of records means that the fields in the record are represented by products of fields.

These are about all we need to work with our fields.

#### Extracting fields

We'll create another type class for extracting fields:

```hs
class ExtractFields rep where
  extractFields :: forall f. f rep -> Array (Tuple String String)
```

This time, we will return an Array of a Tuple of our field name and type.

Let's define our constructor instance:

```hs
instance constructorExtractFields ::
  ( ExtractFields a
  )
  => ExtractFields (Constructor sym a) where
  extractFields _ = extractFields (Proxy :: Proxy a)
```

In the case of extracting rows, we'll want to dig through our top-level constructor to look for fields. The same kind of instances are written for our record and product instances:

```hs
instance recExtractFields ::
  ( ExtractFields fields
  )
  => ExtractFields (Rec fields) where
  extractFields _ =
    extractFields (Proxy :: Proxy fields)

instance productExtractFields ::
  ( ExtractFields a
  , ExtractFields b
  )
  => ExtractFields (Product a b) where
  extractFields _ =
    extractFields (Proxy :: Proxy a) <> extractFields (Proxy :: Proxy b)
```

The only difference for the product being that we want to append together all of the fields that we've gathered.

Now for the two instances that matter:

```hs
instance fieldExtractFields ::
  ( IsSymbol field
  , Generic a rep
  , GTypeName rep
  )
  => ExtractFields (Field field a) where
  extractFields _ =
    pure $ Tuple
      (reflectSymbol (SProxy :: SProxy field))
      (gTypeName (Proxy :: Proxy rep))
```

For the field instance, we can reflect the symbol for our propety name, and then use GTypeName to get the name f the value. Surprisingly simple.

```hs
instance argumentExtractFields ::
  ( GTypeName a
  )
  => ExtractFields (Argument a) where
  extractFields _ =
    pure <<< Tuple "" $
      gTypeName (Proxy :: Proxy a)
```

In the case of our arguments, we'll return an empty string as the property name to use for disambiguation. The extraction here is done with the raw type as above, as the aim is to grab the primitive type inside newtypes.

#### Writing the type definition

As before, our definition ends up being fairly simple. We will need to match if our fields comes back as a single list with an empty string field to see if it is a newtype, but otherwise it is largely the same, but with more content.

```hs
writeTypeDefinition :: forall a rep
  . Generic a rep
  => GTypeName rep
  => ExtractFields rep
  => Proxy a
  -> String
writeTypeDefinition proxy =
  case fst <$> fields of
    [""] -> -- simple newtype check
      "type " <> name <> " = "
        <> (fold $ snd <$> fields)
        <> " // this is a LIE\n"
    _ ->
      "type " <> name <> " = {\n"
        <> contents
        <> "\n}\n"
  where
    name = nameOf proxy
    fields = extractFields (Proxy :: Proxy rep)
    format (Tuple key prop) = "  " <> key <> ": " <> prop
    contents =
      intercalate "\n" $
        format <$> fields
```

Because there's no good way to represent newtypes in TypeScript, I had to come up with a stupid solution: create a type alias with a warning.

## Conclusion

Here is the result of the routes and types output:

```ts
const files: Route<Unused, Path[]> = {
  method: "GET",
  url: "/api/files"
}

const watched: Route<Unused, FileData[]> = {
  method: "GET",
  url: "/api/watched"
}

type Path = string // this is a LIE

type Flag = boolean // this is a LIE

type FileData = {
  path: Path
  watched: Flag
}
```

So there you have it, TypeScript code generation from Purescript!

I hope this has shown that Generic Programming is pretty fun and a powerful way to work with the general structure of your data types.

Please send me your thoughts/corrections on [Twitter](https://twitter.com/jusrin00), thanks!

## Links

[This repo](https://github.com/justinwoo/awful-ps-generics-to-ts-codegen)

[Stephen Diehl's post on GHC Generics](http://www.stephendiehl.com/posts/generics.html)

Cool uses of Generic Programming:

totally forgot to add https://github.com/krisajenkins/elm-export / https://github.com/mattjbray/servant-elm#readme and https://github.com/mattjbray/servant-elm#readme and such

Solutions I've found in various languages:

* [Purescript: Generics-Rep (the one used here)](https://pursuit.purescript.org/packages/purescript-generics-rep/5.1.0)
* [Haskell: Generics](https://hackage.haskell.org/package/base-4.9.1.0/docs/GHC-Generics.html)
* [Scala: Shapeless](https://github.com/milessabin/shapeless)
* [OCaml: Generic](https://github.com/balez/generic)
* [F#: TypeShape](https://github.com/eiriktsarpalis/TypeShape)

