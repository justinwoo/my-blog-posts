---
title: Embedding Elm into a Purescript-Halogen App
tags: purescript Elm
author: kimagure
slide: false
---
**Update: while I wrote this blog post using Generics-Rep, the updated code in my repository uses first-class row type information for records, so no generics are needed at all. I'll try to write a post about this sooner or later.**

This past weekend, I thought it might be fun to try embedding an old Elm app of mine into a Purescript-Halogen one. In this post, I'll go over some of the things I did to make this work, from the basic mounting work and hooking up the top level things in Halogen to some Generic Programming fun I did to validate that my types worked with Elm ports and generate the code I needed for these.

## The base Elm stuff

On the Elm side, I mostly copy-pasted code from a [previous 0.17 implementation](https://github.com/justinwoo/elm-etch-sketch), but ripped out the logic portions and made some adjustments:

1. my module needed to be declared `port module Main`

2. I added these ports:

```hs
port clearScreen : () -> Cmd msg
port modelUpdates : (ElmModel -> msg) -> Sub msg
```

`clearScreen` sends out requests back through the port to ask to clear the screen, and then `moduleUpdates` receives the entire model state through the port so it can update my view accordingly.

3. my Action/Event/Msg/whatnot are now updated accordingly:

```hs
type Msg
  = UpdateModel ElmModel
  | ClearScreen

update : Msg -> ElmModel -> (ElmModel, Cmd Msg)
update msg model =
  case msg of
    UpdateModel newModel ->
      ( newModel, Cmd.none )
    ClearScreen ->
      ( model, clearScreen () )
```

4. my Elm main should be set correctly

This just meant that I needed to hook up the subscription like so:

```hs
main : Program Never ElmModel Msg
main =
  Html.program
    { init = ( init, Cmd.none )
    , update = update
    , view = view
    , subscriptions = (\_ -> modelUpdates UpdateModel)
    }
```

## The Purescript FFI stuff for working with Elm

To work with Elm, we need to call into the global variable that Elm sets for us and start working with it, and then write some functions for the instances. Well, simple enough:

```js
exports.getElmInstance = function (element) {
  return function () {
    return window.Elm.Main.embed(element);
  }
}

exports.subscribeToClearScreen_ = function (instance) {
  return function (push) {
    return function () {
      instance.ports.clearScreen.subscribe(function () {
        push()()
      })
    }
  }
}

exports.sendModelUpdate = function (instance) {
  return function (model) {
    return function () {
      instance.ports.modelUpdates.send(model);
    }
  }
}
```

```hs
foreign import data ElmInstance :: Type
foreign import getElmInstance :: forall eff.
  HTMLElement
  -> Eff (dom :: DOM | eff) ElmInstance
foreign import subscribeToClearScreen_ :: forall eff.
  ElmInstance
  -> (Unit -> Eff eff Unit)
  -> Eff eff Unit
foreign import sendModelUpdate :: forall eff.
  ElmInstance
  -> ElmModel
  -> Eff eff Unit
```

I need to keep track of where my Elm instance is, so I made an opaque foreign data type for it. the rest is just normal stuff, but I chose not to try to track every effect, though, in larger projects you might end up being interested in tracking some kind of ELM/REACT/whatever effect.

## The Halogen stuff

I ended up making a simple single lifecycle component, kind of like the one described in my [Full-Stack Purescript](http://qiita.com/kimagure/items/b576b5bfe370180599f8) post. Not very much new here, as I don't have any inputs or outputs to deal with, and I set my other parts accordingly:

```hs
H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    }
  where
    initialState :: State
    initialState =
      { elmInstance: Nothing
      , etchSketch:
          { cursor: Coords {x: 0, y: 0}
          , points: mempty
          , width: 800
          , height: 600
          , increment: 10
          }
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ HH.ClassName "ui-root"
        ,  HP.ref rootLabel
        ]
        []

    rootLabel = H.RefLabel "root"
```

For my state, I need to keep track of the Elm instance that I'm working with, and I need a convenient way to separate out the actual Etch-Sketch state that I'll be passing into elm. Working with nested records is quite easy in Purescript, so this wasn't too much of a concern.

The rendering portion is about what you'd expect from any component-based solution: I render a blank div and use a ref to keep track of it. This is later used for querying for it in my eval functions.

The queries look something like this:

```hs
data Query a
  = Init a
  | MoveCursor Direction a
  | ClearScreen Unit (H.SubscribeStatus -> a)
  | UpdateElm a
```

`Init` for initializing, `MoveCursor` for moving the cursor in one of four directions, `ClearScreen` being used for the callback from Elm ports for requests to clear my screen, and `UpdateElm` for the query that is used to then send updates of the state back through the Elm port. The `ClearScreen` constructor looks weird just because it needs to handle the callback value and notify the component whether or not the subscription should stay alive, but let's look at that with the actual eval logic of `Init`:

```hs
eval (Init next) = do
  root <- H.getHTMLElementRef rootLabel
  case root of
    Just element -> do
      elmInstance <- H.liftEff $ getElmInstance element
      H.subscribe $ ES.eventSource (subscribeToClearScreen_ elmInstance)
        (Just <<< H.request <<< ClearScreen)
      H.modify _ { elmInstance = Just elmInstance }
    Nothing -> do
      error' "Couldn't get root instance"
```

Here we handle the initialization from the lifecycle. I grab the root element using the ref I defined, and from there actually run the effect to get the elm instance after it has mounted to the appropriate container.

Here we see our use of subscriptions to event sources in Halogen, where we can attach a handler that will then product actions to be send into our component. The handler for `ClearScreen` looks like this:

```hs
eval (ClearScreen _ reply) = do
  H.modify _ {etchSketch {points = mempty :: Array Coords}}
  _ <- eval (UpdateElm reply)
  pure (reply H.Listening)
```

So it clears the points that have been drawn, makes a `UpdateElm` call, and then returns the condition to continue listening to our component, with the `UpdateElm` handler:

```hs
eval (UpdateElm next) = do
  state <- H.get
  case state.elmInstance of
    Just elmInstance -> do
      pure unit
      H.liftEff $ sendModelUpdate elmInstance
        (toElmModel $ ElmModel state.etchSketch)
    Nothing ->
      pure unit
  pure next
```

So whenever we do an `UpdateElm`, it takes our state and uses the instance to then send the model update, wrapping our record type with the newtype having the correct instances to do the "model conversion", which is just a trick. This is where things start to get really fun.

## Certified Safe-For-Elm-Ports data types

One of the reasons why you'd prefer to use ports when dealing with an embedded Elm application if you already have a static, validated way to make data to send is that you don't have to write the decoding code manually for port types. This also means that there's less indirection in having to handle the success/failure cases.

The other reason was that for my Etch-Sketch to not be too sluggish, I needed a way to represent data that Elm could handle through its ports and work with directly, because I would have to deal with the cost of constantly having to serialize my data into Elm-friendly versions.

### Smells like a Generics problem to me

The [Elm docs on Javascript interop](https://guide.elm-lang.org/interop/javascript.html#customs-and-border-protection) describe what types are allowed in ports. Basically, everything that is not first-class in Javascript won't work, so how do we model this? Well, my solution for this is to inspect the Generic representation of my data types and use a type class for constraints:

```hs
class IsElmPortSafe rep

toElmModel :: forall a rep
  . Generic a rep
  => IsElmPortSafe rep
  => a
  -> a
toElmModel = id
```

In this case, I use the constraints to check the types being passed in, and the polymorphic `a -> a` function ensures that I can only define this method with `id`.

*If you're not familiar with Datatype Generics or Generic Programming, you might be interested in my post [here](http://qiita.com/kimagure/items/cc0ea2982abdf1625e87).*

Now for the actual instances, I'll provide instances for things I know will work, like newtypes ("naked" in runtime), Arrays, etc.:

```hs
-- handles "Apple arg" in "newtype Apple = Apple String"
instance isElmPortSafeConstructor :: IsElmPortSafe arg => IsElmPortSafe (Constructor name arg)

-- handles arguments to type representations
instance isElmPortSafeArgument :: IsElmPortSafe inner => IsElmPortSafe (Argument inner)

-- handles records, where it ensures the fields contained work
instance isElmPortSafeRec :: IsElmPortSafe fields => IsElmPortSafe (Rec fields)

-- for a list (inner : fields), makes sure the inner field is safe and then checks the rest
-- normal products are not port-safe, so Field acts as an extra constraint
instance isElmPortSafeProductFields ::
  ( IsElmPortSafe inner
  , IsElmPortSafe fields
  ) => IsElmPortSafe (Product (Field name inner) fields)

-- checks that a field of a record is safe
instance isElmPortSafeField :: IsElmPortSafe inner => IsElmPortSafe (Field name inner)

-- handles arrays and ensures the inner rep is safe
instance isElmPortSafeArray :: IsElmPortSafe inner =>  IsElmPortSafe (Array inner)

-- the Int type is supported through ports
instance isElmPortSafeInt :: IsElmPortSafe Int
```

### Working with the types

Now I can define types using the type class accordingly:

```hs
newtype Coords = Coords
  { x :: Int
  , y :: Int
  }
derive instance genericCoords :: Generic Coords _
instance isElmPortSafeCoords ::
  ( Generic Coords rep
  , IsElmPortSafe rep
  ) => IsElmPortSafe Coords

newtype ElmModel = ElmModel EtchSketch
derive instance genericEtchSketch :: Generic ElmModel _

type EtchSketch =
  { cursor :: Coords
  , points :: Array Coords
  , height :: Int
  , width :: Int
  , increment :: Int
  }
```

With this, I'm ready to handle `UpdateElm` queries:

```hs
eval (UpdateElm next) = do
  state <- H.get
  case state.elmInstance of
    Just elmInstance -> do
      pure unit
      H.liftEff $ sendModelUpdate elmInstance
        (toElmModel $ ElmModel state.etchSketch)
    Nothing ->
      pure unit
  pure next
```

So while our `toElmModel` function does nothing, it still does all the checks to make sure our type is safe!

## Codegen for correctness and coolness

So even though we have port-safe types to send through, we still would end up two separate type definitions that need to be in sync for this to work. Anything that humans have to do manually has a high probability of going wrong, so I like to minimize this as much as possible -- the answer being code generation! And of course, we'll use Generics here also.

To do this, I do something similar to the above, but with a real purpose this time: I want to take a Proxy (a container for the type I'm working with) and produce a String for the code I want to write.

```hs
getElmRep :: forall a rep
  . Generic a rep
  => IsElmPortSafe rep
  => HasElmRep rep
  => Proxy a
  -> String
getElmRep _ = toElmRep (Proxy :: Proxy rep)

class HasElmRep f where
  toElmRep :: Proxy f -> String
```

So here also, I take the Generic rep and check that it's port safe, and then introduce a new constraint to check that the rep is representable as Elm and comes with a `getElmRep` method.

```hs
-- instance for Constructor
instance herConstructor ::
  ( IsSymbol name -- use the symbol name of the type for the type alias name
  , HasElmRep inner -- use the ElmRep of the inner values
  ) => HasElmRep (Constructor name inner) where
  toElmRep _ = "type alias " <> name <> " =" <> contents
    where
      name = reflectSymbol (SProxy :: SProxy name)
      contents = toElmRep (Proxy :: Proxy inner)

-- records will be on a new line and use brackets and this formatting
instance herRec ::
  ( HasElmRep inner
  ) => HasElmRep (Rec inner) where
  toElmRep _ = "\n  { " <> contents <> "\n  }\n"
    where
      contents = toElmRep (Proxy :: Proxy inner)

-- the only products really supported are record fields, but this is fine
instance herProduct ::
  ( HasElmRep a -- check the left side
  , HasElmRep b -- check the right side
  ) => HasElmRep (Product a b) where
  toElmRep _ = first <> "\n  , " <> second
    where
      first = toElmRep (Proxy :: Proxy a)
      second = toElmRep (Proxy :: Proxy b)
```

For the actual fields, I need to bail early and get either the literal type name to be used in Elm or the constructor name of the data type. I introduce another type class for extracting the name here.

```hs
class ExtractName f where
  extractName :: Proxy f -> String

instance epInt :: ExtractName Int where
  extractName _ = "Int"

-- use Lists for Arrays, since Elm will have to parse Arrays also anyway
instance epArray :: ExtractName a => ExtractName (Array a) where
  extractName _ = "List " <> extractName (Proxy :: Proxy a)

-- use an Overlapping instance in lieu of Instance Chains not yet being available
-- alternative available below
instance epZZZ :: -- overlapping instance because i am a madman
  ( Generic a rep
  , TypeEquals rep (Constructor name b)
  , IsSymbol name
  ) => ExtractName a where
  extractName _ = reflectSymbol (SProxy :: SProxy name)

-- this would work fine too, but it's less fun:
instance epCoords :: ExtractName Coords where
  extractName _ = genericExtractConstructorName (Proxy :: Proxy Coords)

genericExtractConstructorName :: forall a rep name b
  . Generic a rep
  => TypeEquals rep (Constructor name b)
  => IsSymbol name
  => Proxy a
  -> String
genericExtractConstructorName _ = reflectSymbol (SProxy :: SProxy name)
```

With this done, all that is left is to generate this and write to a file:

```hs
prepareContents :: String -> String
prepareContents contents = "module EtchSketch.Types exposing (..)\n\n" <> contents

main = launchAff do
  writeTextFile UTF8 "./src/EtchSketch/Types.elm" contents
  log "done"
  where
    contents = prepareContents $
      getElmRep (Proxy :: Proxy Coords) <>
        getElmRep (Proxy :: Proxy ElmModel)
```

Now when we run this Codegen module, it writes to `Types.elm` and that can be used by the Elm compiler! The output for our types looks like this:

```hs
module EtchSketch.Types exposing (..)

type alias Coords =
  { x : Int
  , y : Int
  }
type alias ElmModel =
  { cursor : Coords
  , height : Int
  , increment : Int
  , points : List Coords
  , width : Int
  }
```

And now every time we run our whole build, our Elm types will be generated again and used for compiling in Elm!

## 完成

This is about what all was involved in embedding an Elm app in a Halogen one in a type-safe manner with codegen. We used Generic programming to solve two problems that would have required a lot of error-prone manual work to get a better integration than otherwise possible.

I hope this gives you some ideas on how to approach embedding different kinds of projects like Typescript+React/RN front-ends and other things to Purescript applications. Let me know on [Twitter](https://twitter.com/jusrin00) what you think, or if you have an integration story involving codegen and other approaches.

## Links

* This repo: https://github.com/justinwoo/purescript-halogen-elm-etch-sketch
* My post about Datatype Generic programming: http://qiita.com/kimagure/items/cc0ea2982abdf1625e87

