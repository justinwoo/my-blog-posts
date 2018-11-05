# Easy Hot-Reloading PureScript with Parcel

While some people already have plenty of experience with front-end web tools to choose exactly what they want, many also have asked how to get everything set up to do building and hot-reloading setups with PureScript. This post will go into how you can easily set up a hot-reloading setup in PureScript, especially if you use [PureScript-React-Basic](https://github.com/lumihq/purescript-react-basic).

Tl;dr for experienced people: PureScript compiled output is available in `/output` of the project root as CommonJS modules, under folders of the name of the compiled module.

## Setup

As usual, you may want to run `pulp --psc-package init` in a new directory and install the dependencies as needed, with your `psc-package.json` looking like the following:

```json
{
  "name": "some-ps-thing",
  "set": "pho",
  "source": "https://github.com/justinwoo/package-sets.git",
  "depends": [
    "react-basic",
    "eff",
    "prelude"
  ]
}
```

In this example, I'll be using PureScript-React-Basic, in addition to some other things. So I'll be adding a `package.json` file:

```json

{
  "scripts": {
    "start": "parcel index.html",
  },
  "devDependencies": {
    "parcel-bundler": "^1.7.0",
    "react": "^16.3.1",
    "react-dom": "^16.3.1",
    "react-hot-loader": "^4.0.1"
  }
}
```

Here, I brought in `react-hot-loader` as we'll be using it to hot-reload our React components while keeping their states. For this, we'll need to add a `.babelrc` file with the settings:

```
{
  "plugins": ["react-hot-loader/babel"]
}
```

Then we need an `index.html` file for our entry point.

```html
<div id="app"></div>
<script src="./index.js"></script>
```

And the contents of our `index.js` file will contain some React boilerplate to use our PS-React-Basic component:

```js
import React from "react";
import ReactDOM from "react-dom";

import Main from "./output/Main";

function main() {
  const myComponent = React.createElement(Main.example, { label: "Increment" });

  ReactDOM.render(myComponent, document.getElementById("app"));
}

// see https://parceljs.org/hmr.html
if (module.hot) {
  module.hot.accept(function() {
    console.log("running main again");
    main();
  });
}

console.log("starting");
main();
```

Such that this expects to import in `Main` from our output and take the component from `Main.example`.

## Main.purs

For our example, we'll just define some counter with its own state and some text we'll change around:

```hs
module Main where

import Prelude

import Control.Monad.Eff.Uncurried (mkEffFn1)
import React.Basic (ReactComponent, react)
import React.Basic.DOM as R

type ExampleProps =
  { label :: String
  }

type ExampleState =
  { counter :: Int
  }

example :: ReactComponent ExampleProps
example = react
  { displayName: "example"
  , initialState
  , receiveProps
  , render
  }
  where
    initialState :: ExampleState
    initialState = { counter: 0 }
    receiveProps _ _ _ = pure unit

    render { label } { counter } setState =
      let
        hello =
          -- we'll change "Hello World" around later
          R.h1 { children: [ R.text "Hello World" ]}
        button =
          R.button
            { onClick: mkEffFn1 \_ -> do
                setState \s -> { counter: s.counter + 1 }
            , children:
                [ R.text (label <> ": " <> show counter)
                ]
            }
      in
        R.div
          { children:
              [ hello
              , button
              ]
          }
```

Then we need to either run `pulp build` explicitly or use an editor plugin to automatically do the builds as part of the IDE functionality.

## Putting this to work

Now we can run our defined `npm start` task or `parcel index.html` directly, and our build will finish in 1-2 seconds and start up a server on `localhost:1234` by default.

```
Server running at http://localhost:1234
✨  Built in 921ms.
```

And if we're using an IDE plugin, we can try clicking the counter a few times, edit the "Hello World" text above, and the IDE plugin that you use will either re-build the module immediately or on save, taking roughly 50-200ms to do the compilation and another 100-300ms for Parcel to pick up the changes and load them:

```
Server running at http://localhost:1234
✨  Built in 241ms.
```

![](https://i.imgur.com/9gEuuyU.png)

And that's about it, really. Here's a video of this in action: https://twitter.com/jusrin00/status/985192240418033664

## Conclusion

Hopefully this has shown you how you can easily use existing tools for web front-end development and put them to use by using the CommonJS output from PureScript. You could easily use whichever tools you like, but I've personally been using Parcel both at work and at home to make working with projects require less configuration.

## Links

* This repo: https://github.com/justinwoo/purescript-parcel-example
* Parcel: https://parceljs.org/
* A Halogen version with some hacks: https://github.com/justinwoo/halogen-parcel-hot-reload-demo

