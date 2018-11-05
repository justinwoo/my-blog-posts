# Fun with Records in Haskell by making "RowLists"

Last time, I wrote about my pairing a record of phantom types with a record of functions that were to be used to register route handlers in PureScript [here](https://qiita.com/kimagure/items/bb9bd3e4ffe1bba4c214). I plan to do something similar in Haskell, so I decided to test the waters by trying out writing a pairwise operations demo in Haskell.

I have to say up front that this post doesn't have much value other than being a wacky Haskell-pyramidy grab-bag of tricks that one *might* consider using in their apps, but probably shouldn't be sooooo prevalent in their codebase.

## The setup

Surprisingly, there's very little that I have to implement myself because most of the heavy lifting has already been done by [kcsongor](https://github.com/kcsongor) and [lunaris](https://github.com/lunaris) in [generic-lens](https://github.com/kcsongor/generic-lens). So I will be making heavy use of this library to work with its `HasField` implementation that lets me both get and set fields.

## Getting to work

So there are roughly two things I needed, and I chose to just work backwards:

1. I need a pairwise apply typeclass that takes multiple parameters such that I can use the instance heads to match fields of my records and apply the arguments correctly.
2. I need a function to convert GHC8 generics of record data types to a RowList-like structure.

But really, these are the only things I needed (since this is a "fairly simple" demo after all).

### PairwiseApply

So I wrote this first since I knew I had multiple options on how to convert generics to rowlist (I could even use generics-sop + records-sop if I really needed to). Like in my PureScript approaches, I had four criteria, on which I built my parameters:

1. I need my record of functions (default *-kinded)
2. I need the RowList representation of the functions record generic rep ([(Symbol, *)] kinded)
3. I need my record of values (default *-kinded)
4. I need the RowList representation of the values record generic rep  ([(Symbol, *)] kinded)

Then I have a method for doing the whole shazam, taking the RowLists in proxy params. So my type class is defined as such:

```hs
class PairwiseApply
    functions (functionsList :: [(Symbol, *)])
    values (valuesList :: [(Symbol, *)])
  where
    pairwiseApplyImpl
      :: Proxy functionsList
      -> Proxy valuesList
      -> functions
      -> values
      -> values
```

The nil instance ends up being easy to write, and I make sure that I only handle the nil-nil case since I don't want any mismatches in the number of fields.

```hs
instance PairwiseApply functions '[] values '[] where
  pairwiseApplyImpl _ _ _ x = x
```

The cons instance is made easy by generic-lens' `HasField`, so there's not much other than some mechanical work:

```hs
instance
  ( HasField name (val -> val) functions
  , HasField name val values
  , PairwiseApply functions fnTail values valTail
  ) => PairwiseApply
         functions (('(name, (val -> val))) ': fnTail)
         values ('(name, val) ': valTail) where
  pairwiseApplyImpl _ _ fns vals = do
      setField @name (fn val) vals'
    where
      fn = getField @name fns
      val = getField @name vals
      vals' = pairwiseApplyImpl (Proxy @fnTail) (Proxy @valTail) fns vals
```

The instance heads automatically lining up for me without me spamming TypeEquals everywhere is so nice. Shame that the syntax highlighting from the promoted operators is usually so godawful though.

But you can see that I pull the function and value out, apply the rest of the fields, and then I update the field with the new value on the rest using the function and value.

Then I wrapped this up in a function that I would actually want to use like so:

```hs
pairwiseApply :: forall vals fns valsL fnsL
   . Generic fns
  => Generic vals
  => fnsL ~ GRowToList (Rep fns)
  => valsL ~ GRowToList (Rep vals)
  => PairwiseApply
       fns fnsL
       vals valsL
  => fns
  -> vals
  -> vals
pairwiseApply =
  pairwiseApplyImpl
    (Proxy @fnsL)
    (Proxy @valsL)
```

I was thinking about how to do `GRowToList` using some type classes and fundeps (as I'm not normally writing Haskell, it's how I'd do it in PureScript) while getting ready to go home from Copenhagen, but then Csongor told me I should use his solution instead, and so I wrote it up on the plane.

### RowToList using two families

So while GHC Generics is full of very strange looking names, I mostly only needed to do a couple things:

1. Go through the product produced by GHC Generics (since records are a lie in Haskell anyway)
2. Take the name out of the metadata and the type out of the record argument
3. Unwrap the data type
4. Handle the Unit case as an empty list

And so the type family solution ended up being much nicer:

```hs
type family GRowToList (r :: * -> *) :: [(Symbol, *)] where
  GRowToList (l :*: r)
    = GRowToList l ++ GRowToList r
  GRowToList (S1 ('MetaSel ('Just name) _ _ _) (Rec0 a))
    = '[ '(name, a) ]
  GRowToList (M1 _ m a)
    = GRowToList a
  GRowToList U1 = '[]
```

So taking the rep, it produces exactly the shape I wanted. The one thing I needed an extra family for was the part where I needed to apppend those products together, like so:

```hs
type family (a :: [k]) ++ (b :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
```

And with this, the whole library voodoo part of my demo was finished! My extensions list wasn't so bad either:

```hs
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
```

## Usage

The usage part (thankfully) ends up being very simple, so I defined the extensions up front:

```hs
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
```

And then I defined the types I wanted to use

```hs
data Values = Values
  { a :: Int
  , b :: String
  } deriving (Generic, Show)

data Functions = Functions
  { a :: Int -> Int
  , b :: String -> String
  } deriving (Generic)
```

And as you can see, my labels match up, with the functions being `a -> a` and the values being `a` for each row. The usage then just follows through:

```hs
values :: Values
values = Values
  { a = 1
  , b = "pen"
  }

functions :: Functions
functions = Functions
  { a = (+) 1
  , b = (++) "apple"
  }

main :: IO ()
main = do
  print $ pairwiseApply functions values
  -- output:
  -- Values {a = 2, b = "applepen"}
```

As expected, 1 + 1 is 2, and smashing an apple and a pen together gives you an applepen.

## Conclusion

Hopefully this has shown that you don't have to use Haskell too much to have a lot of fun with weird features (provided that you use a language with similar features). I think I will actually go through sometime and try to put this to use with some Scotty/Spock routing, but don't expect me to have any results with that for another month or two.

Thanks for reading! Please send reactions to me [@jusrin00](https://twitter.com/jusrin00) and most of the questions to Csongor.

### Caveats

Admittedly, the `GRowToList` implementation here doesn't have the one part that the PureScript RowToList has -- it doesn't sort the keys. While implementing type-level sorting might be fun for some, I personally didn't really want to bother when it requires more work and hurts legibility.

## Links

* This demo: https://github.com/justinwoo/records-fields-pairwise
* Random stuff about PureScript's RowLists and RowToList: https://github.com/justinwoo/awesome-rowlist
* An application of this idea in PureScript in my last blog post, "Record-based API Route-Handler pairing with Row Types": https://qiita.com/kimagure/items/bb9bd3e4ffe1bba4c214

