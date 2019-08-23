Previously, I wrote about usage of instance chains when parsing Symbols, but it can be quite difficult to see just in that context. Even though the idea of how instance chains works in PureScript is that they are groups of instances that may have overlapping instance heads, it would help to see an easier example at work.

## "Fun type-level literal number arithmetic with instance chains"

In this post, I wrote about an amusing way to work with type-level numeric literals in PureScript:

<https://qiita.com/kimagure/items/b19cdbc1807109fb11cb>

Here, I defined successions of natural numbers, with an instance chain group so that I could catch overflows of my tiny range of zero to ten.

```hs
class Succ (i :: Symbol) (o :: Symbol) | i -> o, o -> i
instance zeroSucc :: Succ "zero" "one"
else instance oneSucc :: Succ "one" "two"
else instance twoSucc :: Succ "two" "three"
-- ...
else instance noSucc ::
  ( Fail (Text "i don't know how to count any bigger than ten or less than zero")
  ) => Succ i o
```

Then I could define addition and subtraction in a fairly simple way by working by the left operand. For the case of addition, this meant that I wanted to add by simply iterating on the case that given `left`, `right`, and `output`, `left` could be decremented by taking the reverse of succession:

```hs
class Add (l :: Symbol) (r :: Symbol) (o :: Symbol) | l -> r o
instance zeroAdd :: Add "zero" r r
else instance succAdd ::
  ( Succ l' l
  , Succ r r'
  , Add l' r' o
  ) => Add l r o
```

And while this is a really silly example, I hope this gives you some ideas on what you might like to try out if you have a type class with some tricky instances.
