# Expecting Inferred Types (feat. Custom Type Errors)

Because functional dependencies are so useful, I often write libraries and other utilities that can fully determine some type given some input type(s). However, when it comes to testing that the output type has been fully determined, the manual way of testing this by deleting an existing type signature and having the PureScript IDE server generate the type signature is quite cumbersome and error-prone. I sat around and thought about this problem until the solution became clear: use no functional dependencies.

## A Simple Test Case

Say we have a simple class where one parameter can determine the other:

```hs
class SimpleClass a b | a -> b where
  simpleMethod :: Proxy a -> b

instance simpleInstance1 :: SimpleClass Int String where
  simpleMethod _ = "hello"

instance simpleInstance2 :: SimpleClass String Unit where
  simpleMethod _ = unit
```

We can see from the fundeps `a -> b` that instances will be matched based on the type of `a`, which is provided by the `Proxy a` argument of the method `simpleMethod`. We could provide the type `a` in some other forms also, such as in constraints of some other function type signature.

With the instance matched for a concrete type `a`, we will be able to get type `b`, such as the pair `Int, String` and `String, Unit` as above. And we can test that if we create the values of `b` and have the IDE generate the types for us:

```hs
simpleValueInferred = simpleMethod (Proxy :: Proxy Int)

-- No type declaration was provided for the top-level declaration of
-- simpleValueInferred. It is good practice to provide type declarations as a form
-- of documentation. The inferred type of simpleValueInferred was: String in value
-- declaration simpleValueInferred
```

Then with `:PaddType`:

```hs
simpleValueInferred :: String
simpleValueInferred = simpleMethod (Proxy :: Proxy Int)
```

So we can tell that once we have provided an outlet for the type to be produced, PureScript can assign this a concrete type for us to use. This is useful to know, since now we know that we can get the concrete inferred type this way in a let binding.

## Expecting the inferred (determined) type

So let's write our class that takes two parameters and **doesn't** have functional dependencies:

```hs
class ExpectInferred expected actual
```

Ta-da! It's a type class with two parameters where both types are required to match an instance. Yes, it's really not that special.

First, the case when both parameters match:

```hs
instance expectInferredAA :: ExpectInferred a a
```

So this will match when both the first and second parameters are matching. Then the chained instance:

```hs
-- import Prim.TypeError as TE

else instance expectInferredAB ::
  ( TE.Fail
      (TE.Above
         (TE.Text "The expected (first) and actual (second) types did not match:")
          (TE.Beside
            (TE.Text "  ")
            (TE.Above
                (TE.Quote expected)
                (TE.Quote actual))))
  ) => ExpectInferred expected actual
```

To make this easier for myself, I added a custom type error message for when this chained instance is reached. And just to make this class easier to use, I have a convenience method:

```hs
expectInferred
  :: forall expected actual
   . ExpectInferred expected actual
  => Proxy expected
  -> actual
  -> Unit
expectInferred _ _ = unit
```

Note that the expected argument is passed in through a `Proxy`, since we don't want to have to create a value of `expected`, just have its type information ready to inspect.

Great, now I can write test cases for what I expect things to be.

## Usage

As noted above, we need to work with concrete types, so let binding parameters beforehand is important. But as long as we keep that in mind, we can write our test as a simple top-level binding of type `Unit`:

```hs
test1 :: Unit
test1 =
  let
    expectedP = Proxy :: Proxy String
    simpleValue = simpleMethod (Proxy :: Proxy Int)
  in
    expectInferred expectedP simpleValue
```

And this will type check as we enter this into our file with our IDE plugin, so it works!

Then, we can look at an incorrect usage:

```hs
test2 :: Unit
test2 =
  let
    -- this will error correctly:
    expectedP = Proxy :: Proxy String
    -- A custom type error occurred while solving type class constraints:
    --
    --   The expected (first) and actual (second) types did not match:
    --     String
    --     Unit
    --
    -- while applying a function expectInferred
    --   of type ExpectInferred t0 t1 => Proxy t0 -> t1 -> Unit
    --   to argument expectedP
    -- while inferring the type of expectInferred expectedP
    -- in value declaration test2
    simpleValue = simpleMethod (Proxy :: Proxy String)
  in
    expectInferred expectedP simpleValue
```

Since `simpleMethod (Proxy :: Proxy String)` yields a `Unit`, the expected result does not match. Great! Then we can fix this around fairly easily:

```hs
test2 :: Unit
test2 =
  let
    fixedExpectedP = Proxy :: Proxy Unit
    simpleValue = simpleMethod (Proxy :: Proxy String)
  in
    expectInferred fixedExpectedP simpleValue
```

Done!

## Conclusion

Now that I've done and made this a library, I can use it to check some other things I've been working on, like this test for intersection of fields between two RowLists:

```hs
testD :: Unit
testD =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))
    actual = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2, d: "d" }
  in
    expectInferred expected actual
```

Hopefully this post can provide a simple example of the difference between having and not having a functional dependency, custom type errors, instance chains, proxy methods, etc.

## Links

* This code <https://github.com/justinwoo/purescript-expect-inferred>

