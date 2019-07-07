# Making Diffs of differently-typed Records in PureScript

For a while now, there's been a lot of confusion about how to do "diffing" of records in PureScript, a leftover of how people have worked with hashmaps/objects in dynamic langauges. While the vast majority of users who want to "diff" records will either 1) diff two records of the same type (which only involves iteration of the fields) or 2) want to generate a StrMap diff, there's likely a weird third party looking for the most esoteric kind of diff: a diff of two differently-typed records.

This post will talk about my experience of solving the third kind of problem, which is largely useless for most programs, but somewhat interesting to read about.

## What

Say you have a record that maps to a form's inputs, and another record that uses some of that information and others to form another record. These two records will likely share some inputs, of which you may want to create some diff for displaying errors, sending patches to APIs, etc. Probably what we want here is to get the subtype of the two records, and diff only the fields in that subtype.

With that, we know that some of the fields might be different, but won't always be all different. So to build up a collection of the differences, I put these in a List, and since I know statically what the keys should be, and I need to encode these in some kind of annonymous sum, I can use Variant to store these Tuples of the old and new values.

## Getting the intersection

The other day, I wrote about a RowList intersection typeclass [here](https://qiita.com/kimagure/items/fd05ad13ee8def0fb4ed). My usage is in a similar vein, but there some things that this did not solve that I need here:

* My row type for record intersection is not homogeneous, so I can't just match `ty ty` in the instance head. I will have to conditionally match those, for when the labels match.
* My last definition worked even though it didn't correctly solve instances because I knew what concrete subtype I wanted. This time around, I need to fully solve the intersection. This will require some working with how to match the inne intersection.

Other than for these things, I can work primarily the same. For Cons/Nil, Nil/Cons, and Nil/Nil, the intersection will be Nil, as I'm done matching. For Cons/Cons, there's quite a series of lego pieces to throw around, but the overall desires operations goes like this:

* In my instance head, I want to match on the left side name, type, and tail, and the same for the left. The intersection I will match as a whole.
* The ordering is acquired by comparing the symbols. I then create the booleans isEq, isLt, and isEqOrLt for conditions I need to go by.
* In the case that the symbols are matching, I need to make sure that the left type matches both the right type and the type of the head element of the result, along with the constraint that the name matches in the result head. If not, I match against some throwaway bindings.
* In addition, in the matching case, the bound result head bindings will be used to match the head consed to the inner result. Otherwise, the inner result will be equal to the top result.
* In the case that the left symbol is equal or less than, I know that I no longer need the head element of the left list. Otherwise, I will keep iterating with the full list.
* The opposite condition applies to the right list, where I will use the full list if the left is less than, while I use the tail in the other case.
* Finally, I declare the inner constraint using the inner bindings.

This sounds confusing in writing, but thankfully the code is just a matter of typelevel legos.

```hs
class RowListIntersection
  (xs :: RowList)
  (ys :: RowList)
  (res :: RowList)
  | xs ys -> res

instance rliNilXS :: RowListIntersection Nil (Cons name ty tail) Nil
instance rliNilYS :: RowListIntersection (Cons name ty tail) Nil Nil
instance rliNilNil :: RowListIntersection Nil Nil Nil
instance rliConsCons ::
  ( CompareSymbol xname yname ord
  , Equals ord EQ isEq
  , Equals ord LT isLt
  , Or isEq isLt isEqOrLt
  , If isEq xty trashty yty
  , If isEq xty trashty2 zty
  , If isEq (SProxy xname) trashname (SProxy zname)
  , If isEq
      (RLProxy (Cons zname zty res'))
      (RLProxy res')
      (RLProxy res)
  , If isEqOrLt
      (RLProxy xs)
      (RLProxy (Cons xname xty xs))
      (RLProxy xs')
  , If isLt
      (RLProxy (Cons xname yty ys))
      (RLProxy ys)
      (RLProxy ys')
  , RowListIntersection xs' ys' res'
  ) => RowListIntersection (Cons xname xty xs) (Cons yname yty ys) res
```

## Getting the diff

With the hard part done, now we need to write a much simpler class that will iterate the intersection list with the input rows, building up a result row type for our Variant:

```hs
class RecordDiff
  (rl :: RowList) (r1 :: # Type) (r2 :: # Type) (tuples :: # Type)
  | rl -> r1 r2 tuples where
  recordDiff :: RLProxy rl -> { | r1 } -> { | r2 } -> List (Variant tuples)
```

For the base case, we know that once we reach the end of the row list, we're done.

```hs
instance rdNil :: RecordDiff Nil trash1 trash2 () where
  recordDiff _ _ _ = mempty
```

Then for the Cons case, we need to make sure that the field exists in our row types, and add our field of the label and a tuple of the type to our result. Additionally, we need to declare a union of the inner RecordDiff instances with our total result type, so that we can correctly append together our list of subtype Variants to our list of result type Variants. Then in the value level, we choose whether or not a field should be added to the list by if it is not equal in the left and right records.

```hs
instance rdCons ::
  ( IsSymbol name
  , Eq ty
  , RowCons name ty trash1 r1
  , RowCons name ty trash2 r2
  , RowCons name (Tuple ty ty) tuples' tuples
  , Union tuples' trash tuples
  , RecordDiff tail r1 r2 tuples'
  ) => RecordDiff
         (Cons name ty tail)
         r1 r2 tuples
    where
  recordDiff _ r1 r2 =
      first <> rest
    where
      namep = SProxy :: SProxy name
      first
        | l <- get namep r1
        , r <- get namep r2
        , l /= r = pure (inj namep (Tuple l r))
        | otherwise = mempty
      rest = expand <$> recordDiff (RLProxy :: RLProxy tail) r1 r2
```

To make this easier to use, we define a convenience method:

```hs
mismatches :: forall r1 rl1 r2 rl2 rl tuples
   . RowToList r1 rl1
  => RowToList r2 rl2
  => RowListIntersection rl1 rl2 rl
  => RecordDiff rl r1 r2 tuples
  => { | r1 }
  -> { | r2 }
  -> List (Variant tuples)
mismatches r1 r2 = recordDiff (RLProxy :: RLProxy rl) r1 r2
```

## Usage

As the intersection and the result are fully solved, we can just use these with concrete records. For example:

```hs
`test1 :: List
  (Variant
     ( a :: Tuple Int Int
     , b :: Tuple Int Int
     )
  )
test1 = mismatches { a: 1, b: 2 } { a: 2, b: 2 }

test2 :: List
  (Variant
     ( a :: Tuple Int Int
     , b :: Tuple Int Int
     )
  )
test2 = mismatches { a: 1, b: 2 } { a: 2, b: 2, c: "c" }

test3 :: List
  (Variant
     ( a :: Tuple Int Int
     , b :: Tuple Int Int
     )
  )
test3 = mismatches { a: 1, b: 2, c: "c" } { a: 1, b: 3 }

test4 :: List
  (Variant
     ( a :: Tuple Int Int
     , b :: Tuple Int Int
     )
  )
test4 = mismatches { a: 1, b: 2, c: "c" } { a: 2, b: 2, d: "d" }
```

All of these bindings were able to be solved and I only used the auto-suggested type signatures applied to them to write the inferred signatures using `,mis` in Spacemacs.

And then the result come out as expected:

```hs
main :: forall e. Eff ( console :: CONSOLE | e ) Unit
main = do
  traverse_ log' test1
  traverse_ log' test2
  traverse_ log' test3
  traverse_ log' test4
  -- output:
  -- a was different: (Tuple 1 2)
  -- a was different: (Tuple 1 2)
  -- b was different: (Tuple 2 3)
  -- a was different: (Tuple 1 2)
    where
      log' = match
        { a: \x -> log $ "a was different: " <> show x
        , b: \x -> log $ "b was different: " <> show x
        }
```

And that's it! A fully working, statically typed record diff for the specific case when you want to diff two records where you know statically which fields you're interested in.

## Conclusion

Hopefully this has shown that there's a lot of fun stuff you can do with type-level programming for certain kinds of problems. As said before, I think this mostly has concrete utility when you have a record of form input values and a record of the total computed values and whatnot to diff against to show specific warnings around inputs or something, but maybe I'm not quite as imaginative as others. Hopefully if you need something like this, this has given you some ideas on what you'd like to do. But otherwise, you're probably going to just want to make a bunch of StrMaps out of your records and diff them simply that way.

## Links

* Repo: https://github.com/justinwoo/record-diff