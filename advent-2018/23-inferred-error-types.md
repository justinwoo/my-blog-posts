When we write sophisticated type-level code, usually the types should work such that they are fully determined in some way. This means that as long as we give the intermediate a value to correspond to, we'll have the inferred type available to us that we can use the PureScript IDE tooling to insert.

But how do we write a basic assertion that this inferred type should match something, instead of trying to manually verify that types infer to the result we expect?

## "Expecting Inferred Types (feat. Custom Type Errors)"

In this post, I talked about how we can use a simple two-parameter type class to assert that an inferred type should equal an expected type:

<https://qiita.com/kimagure/items/00c1ca57d6999904b595>

The main that this works with by having two instance, with the main instance being that the first and second parameters should be equal. Then an overlapping instance is defined using instance chain groups, which uses the Prim.TypeError module to make a custom Fail constraint, such that we can get an error like so:

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

While many people complain about error messages in PureScript, we can see that the error message here is fairly straightforward if we slow down and read through its parts, telling us that the custom type error occured from the application of the `expectInferred` function. Really, PureScript errors are surprisingly good given the amount of type information we routinely work with, but you have to give it some patience.
