---
title: The Tale of Two Tortellini: making record based libraries in PureScript and Haskell
tags: purescript Haskell
author: kimagure
slide: false
---
Recently, I sat down to write a PureScript library for parsing INI files to nested records as existing approaches used nested string maps, which are one of my least favorite things in the world (see [this](https://twitter.com/jusrin00/status/875238742621028355) meme I made forever ago). While I was able to make the PureScript library with relative ease, and while the Haskell library took more time and effort to make, overall this came out to having just about the same result, though requiring explicit `GHC.Generics`-deriving record data types.

If nothing else, hopefully I can show you that `GHC.Generics` already do most of what you want with minimal or no cost at runtime for the equivalent output to PureScript. Since PureScript has row types and anonymous records, it makes sense that something like `RowToList` exists to facilitate iterating rows, while because Haskell has product data types with meta-selectors for records, it only makes sense to use `GHC.Generics` to construct Generic Representations for those types.

## Background

Let's go over what I consider an "INI" file:

```ini
[section1]
apple=banana
[section2]
grape=kiwi
[section3]
```

This is a valid INI file to me in that

* It has zero to many sections in the document
* It has a section name surrounded with square brackets
* It has zero to many fields in the section
* Each field contains a key to the left of the equal sign, the value to the right

So `Document -> Section -> Field`. Many libraries across all kinds of programming languages represent this as `StrMap (StrMap String)`. But since we're programming with languages where the type information can actually be used to derive code and achieve greater static benefits, I don't want to use these -- especially because using a string map would force me to deal with the potential non-existence of values I want to use and to parse the contents of values when I want to use them -- all checks that should be performed up front. While some people like to throw schema solutions at this problem, this also doesn't work in most languages -- no static information is stored about the validations that have been run in most cases. While we could program with implicit evidence and "just know" what keys are guaranteed to exist and whatnot, I choose not to work with such error-prone techniques. Thus, I parse this `StrMap (StrMap String)` into proper nested records.

## The PureScript version

The parser I wrote using PureScript-String-Parser, which exposes a parsec-like interface for parsing. It parses the input into the nested `StrMap (StrMap String)` structure, so it's not really that interesting.

### Reading the fields

For parsing the fields, I have a type class used to parse the strings into the target types:

```hs
type UhOhSpaghettios = NonEmptyList UhOhSpaghetto -- my errors type

class ReadIniField a where
  readIniField :: String -> Except UhOhSpaghettios a

instance stringReadIniField :: ReadIniField String where -- ...
instance intReadIniField :: ReadIniField Int where -- ...
instance booleanReadIniField :: ReadIniField Boolean where -- ...
instance arrayReadIniField :: ReadIniField a => ReadIniField (Array a) where -- ...
```

This is what is used to read the final `String` at the field level.

### Reading the section body field-wise

For parsing the section body, I use PureScript-Record's Builder to specify in each instance the operation to be performed to the complete compositions of building steps needed. The `Nil` case simply returns the identity of the builder and the `Cons` case looks for the field name existing and attempts to parse the field value from the string retrieved from the `StrMap String`.

```hs
class ReadSection (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  readSection ::
       RLProxy xs
    -> StrMap String
    -> Except UhOhSpaghettios (Builder (Record from) (Record to))

instance nilReadSection ::
  ReadSection Nil () () where
  readSection _ _ = pure id

instance consReadSection ::
  ( IsSymbol name
  , ReadIniField ty
  , ReadSection tail from from'
  , RowCons name ty from' to
  , RowLacks name from'
  ) => ReadSection (Cons name ty tail) from to where
  readSection _ sm = do
    case SM.lookup name sm of
      Nothing ->
        throwError <<< pure <<< ErrorAtSectionProperty name <<< Error
        $ "Missing field in section"
      Just field -> do
        value <- withExcept' $ readIniField field
        rest <- readSection (RLProxy :: RLProxy tail) sm
        let
          first :: Builder (Record from') (Record to)
          first = Builder.insert nameP value
        pure $ first <<< rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP
      withExcept' = withExcept <<< map $ ErrorAtSectionProperty name
```

### Reading the document by sections

Parsing the document overall with the sections is similar to the case with the sections, where each section key is checked for existence in the `StrMap (StrMap String)` and then the retrieved `StrMap String` is then used to read the section body and build the inner record using `Builder`. This is then used to create an insert operation to build up to the total record.

```hs
class ReadDocumentSections (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  readDocumentSections ::
       RLProxy xs
    -> StrMap (StrMap String)
    -> Except UhOhSpaghettios (Builder (Record from) (Record to))

instance nilReadDocumentSections ::
  ReadDocumentSections Nil () () where
  readDocumentSections _ _ = pure id

instance consReadDocumentSections ::
  ( IsSymbol name
  , RowToList inner xs
  , ReadSection xs () inner
  , RowCons name (Record inner) from' to
  , RowLacks name from'
  , ReadDocumentSections tail from from'
  ) => ReadDocumentSections (Cons name (Record inner) tail) from to where
  readDocumentSections _ sm = do
    case SM.lookup name sm of
      Nothing ->
        throwError <<< pure <<< ErrorAtDocumentProperty name <<< Error
        $ "Missing section in document"
      Just section -> do
        builder <- withExcept' $ readSection (RLProxy :: RLProxy xs) section
        let value = Builder.build builder {}
        rest <- readDocumentSections (RLProxy :: RLProxy tail) sm
        let
          first :: Builder (Record from') (Record to)
          first = Builder.insert nameP value
        pure $ first <<< rest
    where
      nameP = SProxy :: SProxy name
      name = reflectSymbol nameP
      withExcept' = withExcept <<< map $ ErrorAtDocumentProperty name
```

### Usage

I defined a function that takes in the string of the INI document to parse into an `Either` of errors (`UhOhSpaghettios`) and the result `Record row` aka `{ | row }`.

```hs
parseIni :: forall rl row
   . RowToList row rl
  => ReadDocumentSections rl () row
  => String
  -> Either UhOhSpaghettios (Record row)
parseIni s = do
  doc <- lmap (pure <<< ErrorInParsing) $ parseIniDocument s
  builder <- runExcept $ readDocumentSections (RLProxy :: RLProxy rl) doc
  pure $ Builder.build builder {}
```

And of course, this being PureScript, this means that I only have to define a concrete *type alias* for a nested record to use this method:

```hs
type TestIni =
  { section1 ::
       { fruit :: String
       , isRed :: Boolean
       , seeds :: Int
       , children :: Array String
       }
  , section2 ::
       { bat :: String
       }
  , "WOWSECTION" ::
       {}
  , "麻婆豆腐" ::
       {}
  }
```

And from the test suite you can see what is expected:

```hs
testDoc :: String
testDoc = """
[section1]
fruit=apple
isRed=true
seeds=4
children=banana,grape,pineapple
[section2]
bat=grey
[WOWSECTION]
[麻婆豆腐]

-- ...
  suite "parseIni" do
    test "works" do
      case parseIni testDoc of
        Left e -> failure $ show e
        Right (result :: TestIni) -> do
          equal result.section1.fruit "apple"
          equal result.section1.isRed true
          equal result.section1.seeds 4
          equal result.section1.children ["banana","grape","pineapple"]
          equal result.section2.bat "grey"
```

And our delicious PureScript Tortellini is done! Nothing more than a simple type alias needed to get this done.

## The Haskell version

So in Haskell, records... are not quite records. They're data types with some meta-selector information that we work with, and people complain about this all the time. But really, even if we can't work with the type information first-class like with PureScript row types, we *do* have a very nice way of working with type information -- `GHC.Generics`.

Surprisingly, many Haskell users I've talked to don't use Generics other than maybe to derive Aeson instances. Considering that being able to program using type information seems like a core tenet of Haskell, more people should be using Generics to tackle problems that they have. But more on this when we get to it.

I first started off by writing a very typical parser with attoparsec. Forgive me that I didn't pick a newer/different library.

### Reading Fields -- more of the same

This part is just the same as the PureScript version, just with one detail: I only made an instance for `Text`. The normal `[Char]` `String` is just not worth supporting.

```hs
class ReadIniField a where
  readIniField :: Text -> Except UhOhSpaghettios a

instance ReadIniField Text where
  readIniField = pure

instance ReadIniField Int where
  readIniField s = case AP.parseOnly AP.decimal s of
    Left e -> throwE (pure . Error . T.pack $ e)
    Right x -> pure x

instance ReadIniField Bool where
  readIniField s
    | T.toLower s == T.pack "true" = pure True
    | T.toLower s == T.pack "false" = pure False
    | otherwise = throwE . pure . Error $
      "expected true/false, got " <> s

instance (ReadIniField a) => ReadIniField [a] where
  readIniField s = traverse readIniField $ T.splitOn "," s
```

### Reading the section body

So for the Haskell version, I needed to build up the Generic Representation of my type that I could then have transformed to give me my concrete record back. In the case of working with Generics of records, there are a few things to know about:

* `M1`: these are meta information proxies used everywhere to construct parts of the rep. These will then have the three varieties:
    * `D1 = M1 D`: datatype metadata
    * `C1 = M1 C`: constructor metadata
    * `S1 = M1 S`: record selector metadata
* `K1`: these are containers for types, such as the concrete types that are in your record parameters.
* `U1`: these are constructors that do not have arguments.

So by following these, we can write our class accordingly to return Generic reps.

```hs
class ReadSection (f :: * -> *) where
  readSection :: HashMap Text Text -> Except UhOhSpaghettios (f a)

-- we only need to apply M1 to the inner type's Rep
instance ReadSection a => ReadSection (D1 meta a) where
  readSection hm = M1 <$> readSection @a hm

-- same as above
instance ReadSection a => ReadSection (C1 meta a) where
  readSection hm = M1 <$> readSection @a hm

-- no arg constructor is the same as itself, nothing else needed
instance ReadSection U1 where
  readSection _ = pure U1

-- read the left and right sides and smash them together as a product
instance
  ( ReadSection a
  , ReadSection b
  ) => ReadSection (a :*: b) where
  readSection hm = (:*:) <$> readSection @a hm <*> readSection @b hm

-- for a given record field, access the known symbol name
-- and read the field into the type t
instance
  ( KnownSymbol name
  , ReadIniField t
  ) => ReadSection (S1 ('MetaSel ('Just name) z x c) (K1 r t)) where
  readSection hm =
    case HM.lookup (T.toLower name) hm of
      Nothing ->
        throwE . pure . ErrorAtSectionProperty name . Error
        $ "Missing field in section"
      Just x -> do
        value <- withExcept' $ readIniField x
        pure $ M1 (K1 value)
    where
      name = T.pack $ symbolVal @name Proxy
      withExcept' = withExcept . fmap $ ErrorAtSectionProperty name
```

So the Haskell version using Generics turned out differently in one major way: instead of being able to iterate through a list, I ended up writing code that instead works on a tree and builds back up from the nodes.

### Reading the document body

And so with similar code to the previous section, the document reading mostly writes itself, where the value is then created from applying `to` to transform the Generic Rep to a value of the type we want:

```hs
class ReadDocumentSections (f :: * -> *) where
  readDocumentSections ::
       HashMap Text (HashMap Text Text)
    -> Except UhOhSpaghettios (f a)

instance ReadDocumentSections a => ReadDocumentSections (D1 meta a) where
  readDocumentSections hm = M1 <$> readDocumentSections @a hm

instance ReadDocumentSections a => ReadDocumentSections (C1 meta a) where
  readDocumentSections hm = M1 <$> readDocumentSections @a hm

instance
  ( ReadDocumentSections a
  , ReadDocumentSections b
  ) => ReadDocumentSections (a :*: b) where
  readDocumentSections hm = (:*:) <$> 
    readDocumentSections @a hm <*> readDocumentSections @b hm

instance
  ( KnownSymbol name
  , Generic t
  , rep ~ Rep t
  , ReadSection rep
  ) => ReadDocumentSections (S1 ('MetaSel ('Just name) z x c) (K1 r t)) where
  readDocumentSections hm =
    case HM.lookup (T.toLower name) hm of
      Nothing ->
        throwE . pure . ErrorAtDocumentProperty name . Error
        $ "Missing field in document"
      Just x -> do
        value <- withExcept' $ to <$> readSection @rep x
        pure $ M1 (K1 value)
    where
      name = T.pack $ symbolVal @name Proxy
      withExcept' = withExcept . fmap $ ErrorAtDocumentProperty name
```

Likewise, the document reading is built up from our tree.

### Usage

In the Haskell version, the top level method is much easier and essentially only requires that our record should have a Generic instance, and works it out from there with our type class:

```hs
parseIni ::
     Generic record
  => ReadDocumentSections (Rep record)
  => Text
  -> Either UhOhSpaghettios record
parseIni s = do
  doc <- first (pure . ErrorInParsing . T.pack) $ parseIniDocument s
  runExcept $ to <$> readDocumentSections doc
```

And so to use this method, we do have to define the concrete record types for each section and the top level document:

```hs
data Config = Config
  { section1 :: Section1
  , section2 :: Section2
  , section3 :: Section3
  , section4 :: Section4
  } deriving (Show, Eq, Generic)

data Section1 = Section1
  { apple :: Text
  } deriving (Show, Eq, Generic)
data Section2 = Section2
  { watermelon :: Bool
  , kiwi :: Int
  } deriving (Show, Eq, Generic)
data Section3 = Section3 {} deriving (Show, Eq, Generic)
data Section4 = Section4 {} deriving (Show, Eq, Generic)
```

But after that, we can very easily put this to use:

```hs
testDoc :: Text
testDoc = intercalate "\n"
  [ "[section1]"
  , "apple=banana"
  , "[section2]"
  , "watermelon=true"
  , "kiwi=1"
  , "[section3]"
  , "[section4]"
  ]
  
-- ...
  describe "Tortellini" $
    it "can parse a test document and has the right values" $
      case parseIni testDoc of
        Left e -> fail (show e)
        Right Config
          { section1 = Section1 {apple}
          , section2 = Section2 {watermelon, kiwi}
          } -> do
          apple `shouldBe` "banana"
          watermelon `shouldBe` True
          kiwi `shouldBe` 1
```

And so the top level usage looks identical to PureScript, where the only difference is that instead of using anonymous record type aliases, Generic-deriving record data types are used instead.

## Conclusion

Hopefully this post was able to show you that many of the same operations done with row types and `RowToList` can be done using `GHC.Generics` in Haskell for record data types that derive `Generic`, and that it doesn't take so much work to use them either.

And if you would like to use Generics in PureScript, or you'd just like to see a simpler take on them, please check out the [PureScript-Generics-Rep](https://github.com/purescript/purescript-generics-rep) library! I've also written plenty about these in previous posts, if you ever need to see some examples of them in use.

Thanks!

## Links

* PureScript-Tortellini https://github.com/justinwoo/purescript-tortellini
* Tortellini https://github.com/justinwoo/tortellini
* GHC Generics docs https://hackage.haskell.org/package/base-4.9.1.0/docs/GHC-Generics.html

