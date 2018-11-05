# Building a mobile web audio player with Purescript-Halogen

For a long time now, I've had this problem that I haven't liked any media players on Android for listening to long files, especially podcasts. While many media players come with a 30-second rewind, they don't give me the level of control I want, which is to be able to skip forward 30 seconds and move around in 5-10 second intervals. Well, what can you do, right?

I had in mind to either do this with React Native and Web. Well, considering I only need to pick a single file, control playback, and target my own Android devices (which are all > 4.4), this wasn't that hard of a decision. This also saved me the trouble of trying to get this published.

I picked [Halogen](https://github.com/slamdata/purescript-halogen) since the way it handles effects in response to queries makes it quite easy for me to do everything I need.

## Building our app

### Types

One thing I found out I needed was ObjectURL hashes, since security features mandate that a cilent's actual filepath to a file is not allowed to be shown. So I made a newtype for that.

```hs
newtype ObjectURL = ObjectURL String
derive instance newtypeFilePath :: Newtype ObjectURL _
```

The derived `Newtype` instance lets me use some other nice functions to allow functions to be applied "over" my newtype. We'll come back to this later.

The actual application state that I care about is only whether or not I have a file to work with, as the actual running state of the audio element is something I really don't want to carry around. This makes for some boring state though.

```hs
type State =
  { file :: Maybe ObjectURL
  }
```

Next, I need my actual Query data type that I'll be using with Halogen. This isn't too sophisticated either:

```hs
data Query a
  = FileSet a
  | Skip SkipDir SkipSize a

data SkipDir = Bck | Fwd
data SkipSize = Sm | Md | Lg
```

When the file is set, I'll be just triggering the action to actually get the file hash to set it.

With the skip, I wanted to make sure a given button has what direction and size of skip defined to emit, so that I can use that to make the effects as needed.

Finally, I have the type of effects that will run in my Halogen component as an `Aff`:

```hs
type AppEffects eff =
  Aff
  ( console :: CONSOLE
  , dom :: DOM
  | eff)
```

### Component definition

As our application only needs to be a simple single component, there are features of Halogen we won't be using here (and that's a good thing!), and our ui definition looks simple enough:

```hs
import Halogen as H
import Halogen.HTML as HH

ui :: forall eff. H.Component HH.HTML Query Unit Void (AppEffects eff)
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where
    -- [...]
```

The Halogen [Component](https://pursuit.purescript.org/packages/purescript-halogen/1.0.0/docs/Halogen.Component#t:Component) definition has a ton of type variables, but there are only three we are actually using here.

1. The first, the type of what we're rendering, which will be HTML.
2. The second, the query algebra, which is the query data type with a parameter we defined earlier.
3. The last (fifth), the monad used for our effects, which is the AppEffects Aff we defined earlier.

The other two are for input and output that are used for parent-child communication that we won't be using here.

The actual spec passed to component seems simple enough: a record with initialState being a function for state using the Unit input we specified, a render function, and eval function, and a receiver function that we don't use here. In the where block, we define what we use here:

#### initialState

```hs
initialState =
  { file: Nothing
  }
```

Unsurprisingly, we have nothing for the file at the start.

#### render

```hs
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

render :: State -> H.ComponentHTML Query
render state =
  HH.div
    [ HP.class_ $ wrap "container" ]
    [ HH.div
      [ HP.class_ $ wrap "root" ]
      [ HH.h1_ [HH.text "glorious web audio thing"]
      , HH.div_
          [ HH.input
            [ HP.ref $ wrap "input"
            , HP.type_ HP.InputFile
            , HP.prop (wrap "accept")  "audio/*"
            , HE.onChange (HE.input_ FileSet)
            ]
          ]
      , HH.div_
          [ HH.audio
            [ HP.ref $ wrap "audio"
            , HP.src $ unwrap $ fromMaybe (wrap "") state.file
            , HP.controls true
            , HP.autoplay true
            ]
          []
        ]
      , HH.div
          [ HP.class_ $ wrap "buttons" ]
          [ HH.button [HE.onClick (HE.input_ $ Skip Bck Lg)] [HH.text "<<<"]
          , HH.button [HE.onClick (HE.input_ $ Skip Bck Md)] [HH.text "<<"]
          , HH.button [HE.onClick (HE.input_ $ Skip Bck Sm)] [HH.text "<"]
          , HH.button [HE.onClick (HE.input_ $ Skip Fwd Sm)] [HH.text ">"]
          , HH.button [HE.onClick (HE.input_ $ Skip Fwd Md)] [HH.text ">>"]
          , HH.button [HE.onClick (HE.input_ $ Skip Fwd Lg)] [HH.text ">>>"]
          ]
      ]
  ]
```

Not much is involved here either, but we should keep note of a few things:

```hs
[ HH.input
  [ HP.ref $ wrap "input"
  , HP.type_ HP.InputFile
  , HP.prop (wrap "accept")  "audio/*"
  , HE.onChange (HE.input_ FileSet)
  ]
]
```

The onchange handler here is set using a HE.input_ function, which will simply return the query `FileSet` to our eval function. Likewise is done for the skip buttons.

#### eval

```hs
eval :: Query ~> H.ComponentDSL State Query Void (AppEffects eff)
```

This is where the meat and bones of our app lives. Let's first see how we handle `FileSet`:

##### FileSet

```hs
eval (FileSet next) = do
  input <- H.getHTMLElementRef $ wrap "input"
  case input >>= fromHTMLElement of
    Just el -> do
      nxs <- H.liftEff <<< files $ el
      case toMaybe nxs >>= toMaybe <<< item 0 of
        Just file -> do
          url <- H.liftEff $ url =<< window
          blob <- H.liftEff $ createObjectURL file url
          prevBlob <- H.gets _.file
          case prevBlob of
            Just x ->
              H.liftEff $ revokeObjectURL (unwrap x) url
            _ -> pure unit
          H.modify \s ->
            s {file = Just <<< wrap $ blob}
        _ -> H.liftAff $ log "No file found"
    _ -> H.liftAff $ log "No input ref found"
  pure next
```

First, we use the ref "input" from earlier to get our element. We then transform it using `fromHTMLElement` into the file input type that we will be using, where if the element parses correctly, then we can continue. Otherwise, nothing is done.

Note that we use `liftEff` to "transform" our `Eff` actions into our Halogen component context.

Once the element has been found and typed, we then use its `files` property to get the first item. We can't use the value due to how the file input is designed. We then take the window.URL property to use URL.createObjectURL to get the ObjectURL we need.

Once we have our ObjectURL, we make sure to revoke the previous ObjectURL so that the browser is explicitly told to remove handlers to the file, and then modify our state to set the file.

This will load up our file to our audio element, allowing it to be played. Let's now look at how skipping is done:

##### Skip

```hs
eval (Skip dir size next) = do
  audio <- H.getHTMLElementRef $ wrap "audio"
  case audio >>= fromHTMLElement of
    Just el -> do
      let el' = htmlAudioElementToHTMLMediaElement el
      current <- H.liftEff $ currentTime el'
      H.liftEff $ setCurrentTime (current + delta) el'
    _ -> H.liftAff $ log "No audio ref found"
  pure next
  where
    skip = case size of
      Lg -> 30.0
      Md -> 10.0
      Sm -> 5.0
    delta = skip * case dir of
      Bck -> -1.0
      _ -> 1.0
```

We grab the "audio" ref to get the element and attempt to type it as above. When we have our audio element, we then convert it to a media element to control it as such.

Using the size and direction, we are able to compute the delta we want, and we then grab the current time of the control, apply our delta, and set the current time of the audio element to the result. That's about it!

### main

```hs
import Halogen.Aff as HA
import Halogen.VDom.Driver as D

main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- D.runUI ui unit body

  log "Running"
```

We use Halogen's runHalogenAff utility to run our Aff, which consists of waiting for the body and running our UI using our ui.

### sw.js and offline

To use offline capabilities, we need to register a service worker and have a manifest file. It ends up being not much more than just some boilerplate:

```html
<!--in head-->
<link rel="manifest" href="manifest.json">

<script>
if('serviceWorker' in navigator) {
  navigator.serviceWorker
    .register('sw.js')
    .then(function() { console.log("Service Worker Registered"); });
}
</script>
```

```json
{
  "name": "My Web Audio Player",
  "short_name": "WebAudio",
  "icons": [{
    "src": "icon-256x256.png",
    "sizes": "256x256",
    "type": "image/png"
  }],
  "start_url": "/purescript-web-audio-player-demo/index.html",
  "display": "standalone",
  "orientation": "portrait",
  "background_color": "#3E4EB8",
  "theme_color": "#2F3BA2"
}
```

```js
self.addEventListener('install', function(e) {
  e.waitUntil(
    caches.open('airhorner').then(function(cache) {
      return cache.addAll([
        './',
        './index.html',
        './dist/app.js',
        './dist/app.css'
      ]);
    })
  );
});
```

## 百聞は一見にしかず

After all that, we have our app looking like this on Android:

![photo_2017-03-05_15-36-00.jpg](https://qiita-image-store.s3.amazonaws.com/0/42481/94bed1f2-b164-a4d7-fdf8-57ee9e8b0d4a.jpeg)

And we can add it to the home-screen like so:

![photo_2017-03-05_15-35-55.jpg](https://qiita-image-store.s3.amazonaws.com/0/42481/3a43941d-e851-e20f-4598-06304650c316.jpeg)

## Conclusion

By using the existing web platform, we were able to build this audio player that works offline on mobile without any native mess or too much work.

I hope this has shown that building simple apps in Purescript is fun. I hope this helps you build your own Purescript apps. Let me know on [Twitter](https://twitter.com/jusrin00) what you think!

Also, please do try it out yourself! https://justinwoo.github.io/purescript-web-audio-player-demo/

## Links

* Repo: https://github.com/justinwoo/purescript-web-audio-player-demo
* Halogen: https://github.com/slamdata/purescript-halogen

