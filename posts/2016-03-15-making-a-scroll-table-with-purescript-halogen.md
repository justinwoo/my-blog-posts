# Making a Scroll Table with Purescript-Halogen

By now I've made this in a variety of ways, using [React](http://qiita.com/kimagure/items/9b7dc73d28550cc3248a), [Cycle.js](http://qiita.com/kimagure/items/d29ed7b7bdaaf6977b9a), and [Elm](http://qiita.com/kimagure/items/57cdd08bdf56cc51d294), so naturally, I thought it'd be fitting to make this with Purescript. But instead of using the reactive-lite version like I did last time, I wanted to dive into the most mature solution for UI development in Purescript: purescript-halogen. This won't really adequately explain all the parts involved, but hopefully pique your interest in learning more about Purescript and purescript-halogen.

## Method

### Vad är "scroll table"?

A scroll table (in my definition) is a table that will only try to display the rows required in the user's current view. I accomplish this by setting a static row height, getting the height of my view, and the current scroll position of my view.

Based on this, my application state type is defined:

```haskell
type VisibleIndices = Array Int

type State =
  { height :: Int
  , width :: Int
  , colWidth :: Int
  , rowCount :: Int
  , rowHeight :: Int
  , visibleIndices :: Array Int
  }
```

And the function for calculating my visible indices is fairly straightforward:

```haskell
calculateVisibleIndices :: State -> Int -> State
calculateVisibleIndices model scrollTop =
  case model of
    { rowHeight, rowCount, height } -> do
      let firstRow = scrollTop / rowHeight
      let visibleRows = (height + 1) / rowHeight
      let lastRow = firstRow + visibleRows

      model { visibleIndices = firstRow..lastRow }
```

Then the initial state of my application can be calculated:

```haskell
initialState :: State
initialState =
  calculateVisibleIndices
    { height: 600
    , width: 900
    , colWidth: 300
    , rowCount: 10000
    , rowHeight: 30
    , visibleIndices: []
    }
    0
```

### Using purescript-halogen

Our main function will be a function of purescript-halogen's effects and is pretty similar to Elm's StartApp bootstrap:

```haskell
main :: Eff (HalogenEffects ()) Unit
main = runAff throwException (const (pure unit)) do
  app <- runUI ui initialState
  appendToBody app.node
```

Now for the fun part: we need to define `ui` to feed in here. First, we need to define our component query.

Halogen components have a query type that they expect, that basically declares the kinds of "actions" that can be sent to the component to handle, which they can use to modify the state of the component. In our scroll table, the only query we care about is the user's scrolling, with the parameter of the scroll position of our view when the user scrolls:

```haskell
data Query a
  = UserScroll Int a
```

Then, combining purescript-css and purescript-halogen-css, I was also able to type my CSS:

```haskell
ui :: forall g. (Functor g) => Component State Query g
ui = component render eval
  where
    render :: State -> ComponentHTML Query
    render state =
      H.div_
        [ H.h1
          [ CSS.style do TextAlign.textAlign TextAlign.center ]
          [ H.text "Scroll Table!!!!" ]
        , H.div_
            [ H.div
                [ P.class_ $ className "container"
                , CSS.style do
                    Display.position Display.relative
                    Geometry.height $ Size.px (toNumber state.height)
                    Geometry.width $ Size.px (toNumber state.width)
                    Overflow.overflowX Overflow.hidden
                    Border.border Border.solid (Size.px 1.0) Color.black
                , E.onScroll $ E.input \x -> UserScroll (getScrollTop x.target)
                ]
                [ tableView state ]
            ]
        ]

    eval :: Natural Query (ComponentDSL State Query g)
    eval (UserScroll e next) = do
      modify $ \s -> calculateVisibleIndices s e
      pure next
```

In building the ComponentHTML, functions with `name_` don't take property lists, whereas `name` do. It's nice to have the distinction in the above. For the scroll event, I use `(Halogen.HTML.Events.)onScroll`, and use `input` in order to define a function that will return a `Query` accordingly. The evaluation step just needs to handle my `UserScroll` query, and just uses `calculateVisibleIndices` to do so. *(If you do want to read an explanation of the type signatures and how they work, probably it is best to read the [purescript-halogen README](https://github.com/slamdata/purescript-halogen/blob/master/README.md))*

I cheated for getting the scrollTop of my element though, as it doesn't seem the HTMLElement type has the property available. No problem though, cheating by using FFI is quite easy:

```haskell
foreign import getScrollTop :: HTMLElement -> Int
```

```js
exports.getScrollTop = function (target) {
  return target.scrollTop;
}
```

The only thing missing now is our `tableView` function that we used above:

```haskell
tableView :: State -> ComponentHTML Query
tableView { rowCount, rowHeight, colWidth, visibleIndices } =
  H.table
    [ CSS.style do Geometry.height $ Size.px (toNumber (rowCount * rowHeight)) ]
    [ H.tbody_ $ map makeRow visibleIndices ]
  where
    makeRow index = do
      let i = toNumber index
      let key = show (i % (toNumber (length visibleIndices)))

      H.tr
        [ P.key key
        , CSS.style do
          Display.position Display.absolute
          Geometry.top $ Size.px (i * (toNumber rowHeight))
          Geometry.width $ Size.pct (toNumber 100)
        ]
        [ H.td
          [ CSS.style do Geometry.width (Size.px (toNumber colWidth)) ]
          [ H.text $ show i ]
        , H.td
          [ CSS.style do Geometry.width (Size.px (toNumber colWidth)) ]
          [ H.text $ show (i * 1.0) ]
        , H.td
          [ CSS.style do Geometry.width (Size.px (toNumber colWidth)) ]
          [ H.text $ show (i * 100.0) ]
        ]
```

And that's it!

## Conclusion

So hopefully I've shown that writing Purescript and using purescript-halogen isn't too terrifying. Also, while I haven't been selling it, it's fairly nice to know that all the inline CSS styles I'm using are valid. But even more than that, every time I compile, I know that everything will work, and the only bugs I need to worry about are logical ones that I've introduced (which can be easily tested with generative testing and such).

If you made it this far, thanks for reading! Please also [tweet](https://twitter.com/jusrin00) me your reactions, criticisms, praises, remarks, comments, responses, suggestions, advice, complaints, thoughts, explanations, corrections, *and/or declarations on the futility of writing in a language that compiles to Javascript instead of Javascript itself or something.*

## Links

* purescript-halogen: https://github.com/slamdata/purescript-halogen
* My repo: https://github.com/justinwoo/purescript-scroll-table