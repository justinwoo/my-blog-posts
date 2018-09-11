# We don't need Peano Numbers in PureScript

It's been a while since I've written anything, since I've been on vacation in Japan. But since I'm bored after spending something like two months cumulatively in Tokyo, I might as well write something to amuse myself and to show the world something really cool/bad -- we don't need peano numbers in PureScript.

## "Motivation"

Why do we need type level numbers in the first place? The simplest demo often presented is a list that has information about its length preserved in types -- from zero to a known N:

```
state:  List with 0 elements
action: add 1 element

state:  List with 1 element
action: remove 1 element

state:  List with 0 elements
```

And usually this is represented in a type such as `data MyList (n :: Something) = ...`

Also, this is an etude, so it's very much doing for its own sake.

## How

Consider the [Prim.Symbol.Append](https://pursuit.purescript.org/builtins/docs/Prim.Symbol#t:Append) type class:

```hs
class Append (left :: Symbol) (right :: Symbol) (appended :: Symbol)
  | left right -> appended
  , right appended -> left
  , appended left -> right
```

There are two operands and one result here, where any two parameters can determine the other based on the functional dependencies. In another way:

```
a <> b == c
  | a b -> c
  , b c -> a
  , a c -> b
```

Are you thinking what I'm thinking?

```
"." <> ".." == "..."
```

Yes, we can use a series of "." Symbols to represent numbers!

## Implementation

### Type Aliases

First, let's make ourselves some convenient aliases:

```hs
type Zero  = ""
type One   = "."
type Two   = ".."
type Three = "..."
type Four  = "...."
type Five  = "....."
type Six   = "......"
type Seven = "......."
type Eight = "........"
type Nine  = "........."
type Ten   = ".........."
```

Awesome.

### Addition

We can define addition as an alias of Symbol Append, with the implicit rule that we can only use periods.

```hs
class Add (l :: Symbol) (r :: Symbol) (o :: Symbol) | l r -> o, l o -> r, r o -> l

instance addInst ::
  ( Symbol.Append l r o
  ) => Add l r o
```

And we can define a function that makes working on proxies easier:

```hs
add :: forall l r o. Add l r o => SProxy l -> SProxy r -> SProxy o
add _ _ = SProxy
```

### Subtraction

Subtraction is a bit more involved, in that we need to flip the arguments around this time when the instance uses Symbol.Append. We should define that the sum of the right operand and the output form the left side, so that effectively we have:

```
r + o     = l
r + o - r = l - r
    o     = l - r
```

And in PureScript:

```hs
class Sub (l :: Symbol) (r :: Symbol) (o :: Symbol) | l r -> o, l o -> r, r o -> l

instance subInst ::
  ( Symbol.Append r o l
  ) => Sub l r o
```

Once again, we can have a convenience function to apply this to proxies:

```hs
sub :: forall l r o. Sub l r o => SProxy l -> SProxy r -> SProxy o
sub _ _ = SProxy
```

### Reflecting to Int

So now we need to be able to reflect the number into a value. For this, we'll make a simple class that iterates up by unconsing a single period at a time:

```hs
class ReflectInt (s :: Symbol) where
  reflectInt :: SProxy s -> Int

instance reflectIntZero :: ReflectInt "" where
  reflectInt _ = 0

else instance reflectIntN ::
  ( Symbol.Cons "." m n
  , ReflectInt m
  ) => ReflectInt n where
  reflectInt _ = 1 + (reflectInt (SProxy :: SProxy m))
```

And we're done!

## Usage

We can apply the classes with proxies and get the correct results inferred, and we can use our type aliases instead to annotate them:

```hs
-- resultAdd :: SProxy "......."
resultAdd :: SProxy T.Seven
resultAdd =
  T.add (SProxy :: SProxy T.Five) (SProxy :: SProxy T.Two)

resultAdd2 :: SProxy "............"
resultAdd2 =
  T.add (SProxy :: SProxy T.Five) (SProxy :: SProxy T.Seven)

-- resultSub :: SProxy ".."
resultSub :: SProxy T.Two
resultSub =
  T.sub (SProxy :: SProxy T.Ten) (SProxy :: SProxy T.Eight)

resultSub2 :: SProxy "....................................."
resultSub2 =
  T.sub (SProxy :: SProxy "...............................................") (SProxy :: SProxy T.Ten)
```

Amazing, right? And we can make a test suite that tests the reflected values:

```hs
main = do
  assert $ T.reflectInt (SProxy :: SProxy T.Zero) == 0
  assert $ T.reflectInt (SProxy :: SProxy "") == 0
  assert $ T.reflectInt resultAdd == 7
  assert $ T.reflectInt resultAdd2 == 12
  assert $ T.reflectInt resultSub == 2
  assert $ T.reflectInt resultSub2 == 37
```

And just in case you don't believe me about that last one:

```
resultSub2 =
  T.sub (SProxy :: SProxy "...............................................") (SProxy :: SProxy T.Ten)
                                                                  |1234567
                                                        |1234567890
                                              |1234567890
                                    |1234567890
                           1234567890
```

## Conclusion

Hopefully I've demonstrated something amusing here that can lead to some more interesting ways to model some information you need to keep track of in your codebase.

## Links

* <https://github.com/justinwoo/purescript-tanghulu>
