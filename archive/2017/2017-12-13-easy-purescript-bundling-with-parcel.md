Easy PureScript bundling with Parcel
Recently, there's been a lot of buzz about a new bundling tool "that could replace/kill Webpack" called Parcel. Well, put shortly, it already works well enough to replace Webpack for my own uses, and you may find you don't need Webpack to build any PureScript projects anymore either.

I installed Parcel Bundler by using `npm i -D parcel-bundler`. You might choose to install it globally if you want.

## Building JS that requires PureScript builds from /output: Done

In my [Bismuth-Example](https://github.com/justinwoo/bismuth-example/tree/ae50fa45e8b38e2fed83248cf408a31d08e76b93) project, I have a index.js file that imports PureScript builds from output like so:

```js
import * as Main from '../output/Main';
```

This then builds in Parcel by using the CLI by running `parcel build ./flow-src/index.js` and outputs to dist/index.js.

The convenient thing about using the output directly is that while file size suffers from Parcel not being able to efficiently compact and remove dead code from the PureScript output, the normal watch operation works instantly. And so if you are using an editor plugin for PureScript IDE, then you will see Parcel rebuild your bundles almost instantly, in my case being 120-140ms.

This is what you would want to work with most of the time anyway, so it's convenient that setup really doesn't require anything.

## Building JS from --optimize built Pulp: Done

Building an optimized PureScript bundle is easy: you specify a output file and use the `-O` flag. Note that you often don't even need this unless you're a real pilkunnussija, and even then you might not care. Either way, if you really do, the easiest solution is to build to a folder *not* named dist as that's where Parcel will want to build to.

So in my [Web-Audio-Player](https://github.com/justinwoo/purescript-web-audio-player-demo/tree/5eeb729806bfc915297974cb693f0fff0e7bd0f7), I run `pulp browserify -O --to build/app.js && parcel build build/app.js` to build my app. Done.

In my [Vidtracker](https://github.com/justinwoo/vidtracker/tree/693e2b6ece5b2b59308c47c69d7beaa3a8b10df8) front end, I also use a similar method by running `pulp build -O -m FrontEnd --to build/index.js && parcel build build/index.js`.

## Conclusion

For my own uses, I will continue using Parcel unless I run into situations where I really need some specific configurations. And really, considering how PureScript can output a normal bundle and can be consumed from the /output files, I will probably not be reaching for Webpack again anytime soon.

## Links

* Parcel https://parceljs.org/
