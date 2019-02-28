With a misleading title, this post will actually be mostly about doing type-level programming to make your own generic solutions.

Many "outsiders" and new PureScript users often have the mindset that given almost any problem X, there should be an existing library for it. And I also think this way about most mainstream languages, where writing your own solution to problems almost always comes with an insurmountable mountain of work. But when you have access to more powerful tools than you're normally afforded, why not try experimenting? While most people think of libraries like [Simple-JSON](https://github.com/justinwoo/purescript-simple-json) as opaque objects they can't actually touch, I'd rather that they think of it as a base they can fork and create derivatives from that more concretely solve their own needs.

And so, while there are quite neat solutions like [Sparkle](https://github.com/sharkdp/purescript-sparkle), which is built on [Flare](https://github.com/sharkdp/purescript-flare), this post will go into some of the methods you will want to use to make your own form library, whether it be as an actual library, or, preferably, part of your application code serving a specific purpose.

## Model

I started with some naive assumptions about how I wanted to start modeling my "form" spec:

* It has some static label that I'm going to use
* It has some data type for what kind of input it will be
* It has some ordering defined separately for which inputs come first

And I chose to represent this with a normal row type of `# Type`:

```hs
type MyFormSpec =
  ( apple :: TextInput
  , banana :: NumberInput
  , cherry :: CheckBox
  )
```

Where the types are defined as opaque data types:

```hs
data TextInput
data NumberInput
data CheckBox
```

Then for the ordering of these inputs, I wanted a type level list of symbols that corresponded to this spec definition, so I went about defining a kind (a "type of type") and two data types (or "constructors") for the kind: a Nil and Cons:

```hs
foreign import kind Ordering
foreign import data OrderingNil :: Ordering
foreign import data OrderingCons :: Symbol -> Ordering -> Ordering
```

I wanted to format these in a nice way, so I defined a type operator alias for the Cons case:

```hs
infixr 1 type OrderingCons as -
```

Then the usage looks like this:

```hs
type MyFormOrdering
  = "cherry"
  - "apple"
  - "banana"
  - OrderingNil
```

## Implementation

Like a regular RowToList solution, I need some way to iterate the ordering that I defined at the type level with the spec information available to grab things out of, which I will use to statically generate my form. But first, I need some way to transport my Ordering information, as PureScript isn't gloriously polykinded. A minor setback.

```hs
data OrderingProxy (o :: Ordering) = OrderingProxy
```

Now we're ready to define our form builder.

### BuildForm

```hs
class BuildForm (xs :: Ordering) (spec :: # Type) where
  buildForm
    :: RProxy spec
    -> OrderingProxy xs
    -> List String -- could be anything else here
```

So the proxies we use here provide the static type information needed to resolve the type class instances, while in runtime they will just be placeholders for the generated static function for List String. I chose to use String here to represent the "elements" you would normally generate, and I leave it up to you to use Halogen, Smolder, jQuery, DOM, etc.

So starting from the base case where our list is empty:

```hs
instance nilBuildForm :: BuildForm OrderingNil spec where
  buildForm _ _ = mempty
```

No more fields to add, no more items. Makes sense, right? Then the normal case:

```hs
instance consBuildFrom ::
  ( RowCons label ty trash spec
  , RenderInput label ty
  , BuildForm tail spec
  ) => BuildForm (OrderingCons label tail) spec where
  buildForm spec _ = first : rest
    where
      first = renderInput (SProxy :: SProxy label) (Proxy :: Proxy ty)
      rest = buildForm spec (OrderingProxy :: OrderingProxy tail)
```

So for each item in the ordering list, I extract out the field at the label for a given type and throw away the subtype information I don't care about. Then I use my `RenderInput` class to render the input, passing along the label and type information, and build the rest of the list accordingly.

### RenderInput

This class is a very normal one where we match on the type and use the label information to reflect to String:

```hs
class RenderInput label ty where
  renderInput
    :: SProxy label
    -> Proxy ty
    -> String -- could be anything else here
```

And so in this demo, I kept these extremely simple:

```hs
instance textInputRenderInput ::
  ( IsSymbol label
  ) => RenderInput label TextInput where
  renderInput _ _ = label <> ": TextInput"
    where
      label = reflectSymbol (SProxy :: SProxy label)

instance numberInputRenderInput ::
  ( IsSymbol label
  ) => RenderInput label NumberInput where
  renderInput _ _ = label <> ": NumberInput"
    where
      label = reflectSymbol (SProxy :: SProxy label)

instance checkBoxRenderInput ::
  ( IsSymbol label
  ) => RenderInput label CheckBox where
  renderInput _ _ = label <> ": CheckBox"
    where
      label = reflectSymbol (SProxy :: SProxy label)
```

And that's actually everything!

## Usage

To use the form builder, I pass the proxies with the types I defined earlier and get the results expected:

```hs
type MyFormSpec =
  ( apple :: TextInput
  , banana :: NumberInput
  , cherry :: CheckBox
  )

type MyFormOrdering
  = "cherry"
  - "apple"
  - "banana"
  - OrderingNil

main = do
  let
    form = buildForm
      (RProxy :: RProxy MyFormSpec)
      (OrderingProxy :: OrderingProxy MyFormOrdering)
    form' = intercalate "\n" form
  log form'
  -- cherry: CheckBox
  -- apple: TextInput
  -- banana: NumberInput
```

Pretty nice, right?

## Conclusion

I hope this has shown you that if you have a problem that you want to solve generically, you have some various tools to be able to do it without losing static type information. Just because there's no solution already there doesn't mean it's not possible! Conversely, I hope you'll become more comfortable with forking or creating derivatives of libraries you like but don't quite agree on some details about, especially Simple-JSON.

Finally, I hope this has shown that just because a library seems to solve a concrete problem, it doesn't mean that this doesn't show a method for solving other concrete or generic problems. The same methods to solve problems in Simple-JSON also apply to [Tortellini](https://github.com/justinwoo/purescript-tortellini), my library for reading INI files. I hope you'll find uses for things in this article for problems not involving forms at all!

## Extra

If you're relatively new to generic row types and to the RowCons type class, you might read through my slides on Simple-JSON with generic Record operations [here](https://speakerdeck.com/justinwoo/easy-json-deserialization-with-simple-json-and-record) and my slides about RowToList [here](https://speakerdeck.com/justinwoo/rowlist-fun-with-purescript-2nd-edition).

## Links

* This repo: http://github.com/justinwoo/pretend-generic-form