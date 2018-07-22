# Fun type-level literal number arithmetic with instance chains

With PureScript 0.12's instance chains, we can now write a lot more "type-level programs" by writing "overlapping" instances of different "concretely" specified parts. I took advantage of this the other day to write some type-level literal number arithmetic.

## Numbers and Succ

As someone who doesn't know math, I decided that numbers in my case would be the actual literal word, e.g. `"one"`, `"two"`, `"three"`, etc, and be associated to each other by a Successor pair. Using instance chains, I was able to make overlapping instances where either the succeeding or preceding could be used to determine the instance:

```hs
class Succ (i :: Symbol) (o :: Symbol) | i -> o, o -> i
instance zeroSucc :: Succ "zero" "one"
else instance oneSucc :: Succ "one" "two"
else instance twoSucc :: Succ "two" "three"
else instance threeSucc :: Succ "three" "four"
else instance fourSucc :: Succ "four" "five"
else instance fiveSucc :: Succ "five" "six"
else instance sixSucc :: Succ "six" "seven"
else instance sevenSucc :: Succ "seven" "eight"
else instance eightSucc :: Succ "eight" "nine"
else instance nineSucc :: Succ "nine" "ten"
else instance noSucc ::
  ( Fail (Text "i don't know how to count any bigger than ten or less than zero")
  ) => Succ i o
```

Since this approach doesn't handle non-literal matches, I chose to set a limit. In reality, one could go about code generating as many numbers as they want, but a "realistic" option probably ends at 100 or so.

## Addition

With this representation, now we can do some addition. In my lazy thoughts, addition of two numbers can accomplished in terms of `Succ` by going by the following conditions:

* When the left operand is zero, we can return the right as the result.
* When the left operand is not zero, we can decrement the left operand, increment the right operand, and repeat with the intermediate results.

So with this naive approach, we can represent the class as having the functional dependency of the left operand determining the right operand and output in this way:

```hs
class Add (l :: Symbol) (r :: Symbol) (o :: Symbol) | l -> r o
instance zeroAdd :: Add "zero" r r
else instance succAdd ::
  ( Succ l' l
  , Succ r r'
  , Add l' r' o
  ) => Add l r o
```

And to make this easy to apply, I defined a function that could be called with proxies:

```hs
add
  :: forall l r o
   . Add l r o
  => SProxy l
  -> SProxy r
  -> SProxy o
add _ _ = SProxy
```

## Subtraction

For subtraction, I went with a similar approach, but this time letting the right operand do the driving:

* When the right operand is zero, we can return the left as the result.
* When the right operand is not zero, we can decrement both the left and right operands and repeat.

```hs
class Sub (l :: Symbol) (r :: Symbol) (o :: Symbol) | r -> l o
instance zeroSub :: Sub l "zero" l
else instance succSub ::
  ( Succ l' l
  , Succ r' r
  , Sub l' r' o
  ) => Sub l r o
```

I also defined a function to make this easy to call:

```hs
subtract
  :: forall l r o
   . Sub l r o
  => SProxy l
  -> SProxy r
  -> SProxy o
subtract _ _ = SProxy
```

## Usage

First, we can just try the functions directly:

```hs
result1 :: _
result1 =
  K.add
    (SProxy :: SProxy "two")
    (SProxy :: SProxy "three")

result2 :: _
result2 =
  K.subtract
    (SProxy :: SProxy "nine")
    (SProxy :: SProxy "three")
```

When you then expand those wildcards using your IDE plugin, you'll get the results:

```hs
result1 :: SProxy "five"
result1 = -- ...

result2 :: SProxy "six"
result2 = -- ...
```

Cool! Now I don't have to do arithmetic by hand anymore!

We can also write some generic functions that will use the `Add` and `Sub` classes directly:

```hs
letsAdd
  :: forall l r o
   . K.Add l r o
  => IsSymbol l
  => IsSymbol r
  => IsSymbol o
  => SProxy l
  -> SProxy r
  -> Effect Unit
letsAdd l r = log s
  where
    o = K.add l r
    s = format (SProxy :: SProxy "{l} plus {r} is {o}")
      { l: reflectSymbol l
      , r: reflectSymbol r
      , o: reflectSymbol o
      }

letsSubtract
  :: forall l r o
   . K.Sub l r o
  => IsSymbol l
  => IsSymbol r
  => IsSymbol o
  => SProxy l
  -> SProxy r
  -> Effect Unit
letsSubtract l r = log s
  where
    o = K.subtract l r
    s = format (SProxy :: SProxy "{l} minus {r} is {o}")
      { l: reflectSymbol l
      , r: reflectSymbol r
      , o: reflectSymbol o
      }
```

Of course, if we try to call these with numbers that would be too big or negative, we'll get errors in compile time:

```hs
  -- correctly errors: i don't know how to count...
  -- letsAdd (SProxy :: SProxy "three") (SProxy :: SProxy "eight")

  -- correctly errors: i don't know how to count...
  -- letsSubtract (SProxy :: SProxy "two") (SProxy :: SProxy "three")
```

But normally, the results will be what we expect:

```hs
main = do
  log "we can do some arithmetic!"
  letsAdd (SProxy :: SProxy "two") (SProxy :: SProxy "three")
  letsSubtract (SProxy :: SProxy "five") (SProxy :: SProxy "three")

  -- result
  -- we can do some arithmetic!
  -- two plus three is five
  -- five minus three is two
```

## Conclusion

Hopefully this has shown you that you can very readily use Symbols and instance chains together and come up with simple solutions to problems where previously some hacky overlapping instances may have been used.

## Links

* Repo <https://github.com/justinwoo/purescript-kazunoko>
