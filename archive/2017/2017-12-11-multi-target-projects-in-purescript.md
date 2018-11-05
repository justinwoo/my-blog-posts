# Multi-Target Projects in PureScript

For various reasons, people coming to PureScript from other languages might feel the need to create a project structure that includes several separate "subprojects" and the like to separate packages from each other while simultaneously needing to refer to the same resources. In PureScript, you don't need to do this if you use `pulp build -O/--optimize` as written about [in the Pulp docs](https://github.com/purescript-contrib/pulp#optimising-code-size).

Tl;dr: if you use pulp build -O, you can depend on any other modules you have and you will automatically only import parts that you need. The end.

## My Example Project

I've had this vidtracker project for a while now that I use to track whether or not I've watched some shows that I've downloaded. This app comes with this structure:

```
src
├── FrontEnd
│   ├── Chart.purs
│   └── Style.purs
├── FrontEnd.purs
├── GenerateStylesheet.purs
├── GetIcons.purs
├── Main.js
├── Main.purs
├── Routes.purs
└── Types.purs
```

And so contained is the code for my frontend, backend, static stylesheet generator, and icon fetcher. I then have the following scripts set up for my builds:

```json
  "scripts": {
    "build:get-icons": "pulp build -O --main GetIcons --to get-icons.js",
    "build:be": "pulp build -O --to index.js",
    "build:fe": "pulp build -O -m FrontEnd --to dist/frontend-prebundle.js && webpack -p",
    "build:generate-css": "pulp run -m GenerateStylesheet",
    "build": "npm run build:fe && npm run build:be && npm run build:get-icons && npm run build:generate-css"
  }
```

So for the cases where I built my JS beforehand, I use `-O` to optimize the builds.

### The FrontEnd

From my project files, my front end pulls in from FrontEnd.Chart, FrontEnd.Style, Routes, and Types. For the most part everything is used, except for my static stylesheet defined using PureScript-CSS in [FrontEnd.Style](https://github.com/justinwoo/vidtracker/blob/a6b684b7cb903a2ffdd5c4764d746f8fb7ca2a72/src/FrontEnd/Style.purs#L63). And so while the stylesheet is defined and exposed in this module, this unused stylesheet from the FrontEnd.purs target does not pull in the export, as can be seen from the JS output:

```js
(function(exports) {
  // module definition
  exports["Container"] = Container;
  exports["Heatmap"] = Heatmap;
  exports["FilterWatched"] = FilterWatched;
  exports["RefreshFiles"] = RefreshFiles;
  exports["GetIcons"] = GetIcons;
  exports["File"] = File;
  exports["Watched"] = Watched;
  exports["Dot"] = Dot;
  exports["FilterLink"] = FilterLink;
  exports["DeleteLink"] = DeleteLink;
  exports["DeleteConfirmation"] = DeleteConfirmation;
  exports["FileLink"] = FileLink;
  exports["FileButton"] = FileButton;
  exports["FileNote"] = FileNote;
  exports["container"] = container;
  exports["filterWatched"] = filterWatched;
  exports["refreshFiles"] = refreshFiles;
  exports["getIcons"] = getIcons;
  exports["file"] = file;
  exports["dot"] = dot;
  exports["filterLink"] = filterLink;
  exports["deleteLink"] = deleteLink;
  exports["deleteConfirmation"] = deleteConfirmation;
  exports["fileLink"] = fileLink;
  exports["fileButton"] = fileButton;
  exports["fileNote"] = fileNote;
  exports["gcn"] = gcn;
  exports["scn"] = scn;
})(PS["FrontEnd.Style"] = PS["FrontEnd.Style"] || {});
```

This is a recurring theme in this post, so be prepared to be bored.

### Backend

Unsurprisingly, my BackEnd code doesn't import anything else from the project other than Types and Routes.

### GetIcon

This one is the most interesting example, where this actually does import from the FrontEnd:

```hs
import FrontEnd (extractNameKinda)
```

Which come from this code in my FrontEnd module:

```hs
nameParser :: Parser (List Char)
nameParser = do
  -- extracts out the name
  
extractNameKinda :: Path -> Either String String
extractNameKinda (Path s) =
  bimap show (fromCharArray <<< filter' <<< fromFoldable) $ runParser nameParser s
  where
    filter' =
      (=<<) case _ of
        '.' -> mempty
        x -> pure x
```

So when I look at my optimize-built get-icons.js, I see the entry for FrontEnd really just has these two functions:

```hs
(function(exports) {
  // a whole bunch of imports
  var nameParser = // generated JS
  var extractNameKinda = // generated JS
  exports["nameParser"] = nameParser;
  exports["extractNameKinda"] = extractNameKinda;
})(PS["FrontEnd"] = PS["FrontEnd"] || {});
```

And yeah, that's really it. There's no extra setup needed, you just need to set up your project and use `pulp build -O`.

Screenshot of this for your consumption:

![example1.png](https://qiita-image-store.s3.amazonaws.com/0/42481/92e433fd-500c-4cd4-a3be-8f1629f1ecb0.png)

## Conclusion

Hopefully this has shown you that you really don't have to play some kind of game with trying to set up a multi-project directory structure to make sure you don't have some redundant/useless code in your outputs. This has already been built into the PureScript compiler and is readily usable from Pulp.

## Links

* Pulp docs on optimize https://github.com/purescript-contrib/pulp#optimising-code-size
* My "full-stack" project https://github.com/justinwoo/vidtracker

