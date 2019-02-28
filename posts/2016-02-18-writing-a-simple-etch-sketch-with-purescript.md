*This post was written for purescript-pux 0.2.x, and 1.0.0 has come out with some major changes. Parts of this post may be very different from what is currently in the library. I've updated some of the sections below, so this should be fairly up-to-date with my repository.*

This will be just a high-level walkthrough of the kinds of things I ran into and enjoyed while making a simple Etch-A-Sketch demo with PureScript. My only hope is that this post will make you curious enough about trying out PureScript (or Elm), and is targeted at Javascripters who want to learn something new or FPers who want to read something that will make them cringe.

Also, I am not a FP wizard. I'm a failed physicist/materials scientist who punches keyboards for a living. If I can write PureScript, surely most of you can write it too. Well, provided you don't just throw in the towel after two minutes.

## Getting Started

Pulp is quite good, no real complaints here. You use Bower for packages since it will reliably flatten, dedupe, force conflict resolution, and install fairly quickly. Kind of refreshing after using a lot of npm. `pulp init`, `bower i`, and you're good to go.

You'll also end up setting up a `index.html` to load whatever you build your PS code into. Not really news though. Typically you'll have nothing more than a `#app` div and a `<script>` tag for your build.

## Code

I guess there's really something like three things to be talked about here: 1) my PS code, 2) code in the "ecosystem", 3) my FFI code.

### My PureScript code

I choose not to be too fancy with my data, though it may seem sufficiently complicated compared to vanilla JS. I'll give some watered-down explanations for what is happening below.

```haskell
data Direction
  = Up
  | Down
  | Left
  | Right

data Coords = Coords Int Int
instance eqCoords :: Eq Coords where
  eq (Coords ax ay) (Coords bx by) = ax == bx && ay == by

data Action
  = MoveCursor Direction
  | ClearScreen
  | NoOp

type State =
  { cursor :: Coords
  , points :: Array Coords
  , width :: Int
  , height :: Int
  , increment :: Int
  }
```

`Direction` is an [Algebraic Data Type](https://wiki.haskell.org/Algebraic_data_type) for directions my cursor will move. Nothing really new here. Basically, if I use any of the members like `Up` in my code, it will construct an object of the data type.

`Coords` is a type with a type constructor of the same name that takes two Int args. Basically, `new Coords(1, 2)` or something (*cue wince for FPers*). Then you have the type class `Eq`, which I declare an instance of it for my `Coords` type, so that whenever the `=` equality operator is used, it uses my definition to figure out what is equal or not. This becomes useful when trying to prevent duplicates of coordinates from piling up later.

`Action` is an ADT for the kind of actions my application will perform. This is the same as the Elm architecture, if you're at all familiar with it. Basically, the whole Redux thing where you do `{type: 'myAction'}`, except with much stronger guarantees.

`State` is just a type alias for a record that at least contains the fields I've specified. Shouldn't be too hard to understand.

Based on what I've explained here, I think most of this code will be self-explanatory:

```haskell
isInvalidPoint :: State -> Coords -> Boolean
isInvalidPoint state (Coords x y) =
  x < 0 || (state.increment * x) > (state.width - state.increment) ||
  y < 0 || (state.increment * y) > (state.height - state.increment)

insertPoint :: Coords -> Array Coords -> Array Coords
insertPoint point points =
  case Array.elemIndex point points of
    Just _ -> points
    Nothing -> Array.cons point points

moveCursor :: Direction -> State -> State
moveCursor direction state =
  case state.cursor of
    Coords x y -> do
      let points' = insertPoint state.cursor state.points
      let cursor' =
        case direction of
            Up -> Coords x (y - 1)
            Down -> Coords x (y + 1)
            Left -> Coords (x - 1) y
            Right -> Coords (x + 1) y
      if isInvalidPoint state cursor'
        then state
        else state {cursor = cursor', points = points'}
```

This is basically all of the logic for how the cursor movements work logically, and how the trail of points are put together. The thing you might find confusing is why I have a case statement for `Array.elemIndex`: that's because that function returns a `Maybe Int`, which represents a sort of box for either an integer for where the equivalent item is in my array, or just the fact that there's no matching value in my array. And of course, you want to open boxes, so this is how you can open a box in PureScript (and Elm).

## Using PureScript libraries

I'm more familiar with reactive programming techniques now for handling asynchronous effects than anything else nowadays, so I chose to use `purescript-signal`, and a React-wrapper library that uses signals for just about everything, `purescript-pux`.

I've updated my view code for pux 1.0, and now it looks much simpler:

```hs
pointView :: Int -> String -> Coords -> Html Action
pointView increment subkey (Coords x y) =
  rect
    [ key (subkey ++ show x ++ "," ++ show y)
    , width (show increment)
    , height (show increment)
    , (Pux.Html.Attributes.x (show $ x * increment))
    , (Pux.Html.Attributes.y (show $ y * increment))
    ]
    []

view :: State -> Html Action
view state =
  let
    pointView' = pointView state.increment
    cursor = pointView' "cursor" state.cursor
    points = map (pointView' "pointView") state.points
  in
    div
      []
      [ div
        []
        [ button
          [ onClick (const ClearScreen) ]
          [ text "Clear" ]
        ]
      , div
        []
        [ svg
          [ style { border: "1px solid black" }
          , width (show state.width)
          , height (show state.height)
          ]
          $ snoc points cursor
        ]
      ]
```

If you've used Elm with Elm-Html before, then you'll be fairly familiar with this (`snoc` is a backwards `cons` for appending an element to an array).

Most of the important part follows as such:

```haskell
update :: Action -> State -> State
update (MoveCursor direction) state =
  moveCursor direction state
update ClearScreen state =
  state { points = [] }
update NoOp state =
  state


foreign import keydownP :: forall e c. (c -> Signal c) -> Eff (dom :: DOM | e) (Signal Int)

keydown :: forall e. Eff (dom :: DOM | e) (Signal Int)
keydown = keydownP constant

keyDirections :: Int -> Action
keyDirections keyCode =
  case keyCode of
    38 -> MoveCursor Up
    40 -> MoveCursor Down
    37 -> MoveCursor Left
    39 -> MoveCursor Right
    _ -> NoOp

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL, dom :: DOM | e) Unit
main = do
  keydown' <- keydown
  app <- start
    { initialState: initialState
    , update: fromSimple update
    , view: view
    , inputs:
      [ keyDirections <~ keydown'
      ]
    }

  renderToDOM "#app" app.html
```

`main` is the main function that will be called for my application, and then in there, `keydown'` is the identifier to which I bind my channel that has side effects associated with it. You can see from the signature that `main` will have the following kinds of side effects associated with it: Signal CHANNEL, DOM, EXCEPTION, and some contained arbitrary effects. This has also become simpler with pux 1.0, where you can define some simpler signatures for what your application will do.

You'll see the `keyDirections` that is mapped for the `keydown'` signal returns `Action`s, as they will be used in my update function.

### FFI

The line with the `foreign import` might look rather weird. What is happening is that I needed to get the actual `keydown` event keycodes from `window`, but there wasn't anything in `purescript-signal` readily usable for that. Well, no problem, I know how to write Javascript, still.

I made a `Main.js` with these contents:

```js
// module Main

exports.keydownP = function (constant) {
  var out = constant(0);

  window.addEventListener('keydown', function (e) {
    out.set(e.keyCode);
  });

  return function () {
    return out;
  }
}
```

And, just for reminder, this is how I imported and used it in my PureScript code:

```haskell
foreign import keydownP :: forall e c. (c -> Signal c) -> Eff (dom :: DOM | e) (Signal Int)

keydown :: forall e. Eff (dom :: DOM | e) (Signal Int)
keydown = keydownP constant
```

So this foreign function takes an argument for a function that takes a value and will return a signal of that type, and then it will return a Signal of Int. Then I call it with Signal.constant (which has the signature that matches the argument of the foreign function) and get back a signal of integers.

The JS side uses the JS Signals that come from the FFI that is in `purescript-signal`. It's basically an Rx Subject, where you can push values in as you want.

## Conclusion

So this has been a whirlwind tour of what all was involved in making my Etch-Sketch demo. Hopefully it has demonstrated that most of this code is very legible, and not very hard to understand. And even better, the compiler will make sure that there are no mistakes like type mismatches. This prevents pretty much all runtime exceptions and ensures that the code that compiles will actually work.

Let me know on [Twitter](https://twitter.com/jusrin00) if this was at all helpful or just a pile of crap. Just about any feedback is welcome! (You can even tell me you'll just use FlowType, and I will just frown but be happy for you)

(I'll also write an Elm version of this article in a week or so)

## "References"

* My PureScript Etch-Sketch repo https://github.com/justinwoo/purescript-etch-sketch
* PureScript Language Guide https://github.com/purescript/purescript/wiki/Language-Guide
* purescript-signal https://github.com/bodil/purescript-signal
* purescript-pux https://github.com/alexmingoia/purescript-pux
* PureScript by Example https://leanpub.com/purescript/read