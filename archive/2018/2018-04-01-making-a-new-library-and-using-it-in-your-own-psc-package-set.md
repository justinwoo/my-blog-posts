# Making a new library and using it in your own Psc-Package set

For two years now, I've been writing PureScript-Node programs that need to spawn a child process and read the output out, and this whole time, I've been writing out the same code over and over for no real good reason. Recently, I finally decided to extract this repeated code out to a library, so I wouldn't have to maintain all the different versions anymore.

Well, might as well finally make a library for this, right?

## PureScript-Sunde

As this is nothing more than a sausage-grinding library, I decided to name it after a Korean blood sausage. The library code does little more than 1) spawn a child process of a given command, its args, and purescript-node-child-process arguments, 2) read the stdout and stderr event contents to a ref, and 3) return the results and the exit signal out as a record in an Aff, so I can reasonably write programs that use the results of these asynchronous processes. The entire source code fits in one block:

```hs
spawn'
  :: Encoding
  -> Signal
  -> String
  -> Array String
  -> CP.SpawnOptions
  -> Aff _
      { stdout :: String
      , stderr :: String
      , exit :: CP.Exit
      }
spawn' encoding killSignal cmd args options = makeAff \cb -> do
  stdoutRef <- newRef ""
  stderrRef <- newRef ""

  process <- CP.spawn cmd args options

  onDataString (CP.stdout process) encoding \string ->
    modifyRef stdoutRef $ (_ <> string)

  onDataString (CP.stderr process) encoding \string ->
    modifyRef stderrRef $ (_ <> string)

  CP.onError process $ cb <<< Left <<< CP.toStandardError

  CP.onExit process \exit -> do
    stdout <- readRef stdoutRef
    stderr <- readRef stderrRef
    cb <<< pure $ {stdout, stderr, exit}

  pure <<< effCanceler <<< void $ CP.kill killSignal process
```

Basically the equivalent of what you would have written out in normal JS also, but using the typed API from purescript-node-child-process, with the correct cancellation logic for killing the child process if the Aff has been cancelled (e.g. in a parallel race application).

Then I publish the library as usual with pulp to Pursuit: <https://github.com/justinwoo/purescript-sunde>

## Add it to your package set

If you use Psc-Package, you should probably have your own fork of the package-sets. In my own, I have a commit that I amend where I rebase my commit based on the upstream changes, so I applied the change to my applied commit here: <https://github.com/justinwoo/package-sets/commit/12623e2b5ae767e24cb563917620349cd692d25f>. My workflow for updating my package goes something like this:

```bash
cd my-package-set
gupstream # git fetch upstream master && git rebase FETCH_HEAD
          # where "upstream" is purescript/package-sets

# I edit my packages.json by hand or with scripts
# Examples you might refer to:
# * get-package-def [1]
# * prepare-bower.pl and add-from-bower.pl [2]

pp format
pp verify sunde # verify this package builds with its specified dependencies
gcane # git commit --amend --no-edit
gpo -f # git push -u origin $(git rev-parse --abbrev-ref HEAD)
       # where "origin" is justinwoo/package-sets
       # and the result of the subexpression is my current branch name
git tag something
git push origin something
```

* [1] [get-package-def](https://github.com/justinwoo/get-package-def)
* [2] [prepare-from-bower.pl](https://github.com/justinwoo/spacchetti/blob/951b733f7309e4a8a23a06aaaff3fc96391c5a04/prepare-bower.pl), [add-from-bower.pl](
https://github.com/justinwoo/spacchetti/blob/951b733f7309e4a8a23a06aaaff3fc96391c5a04/add-from-bower.pl)

Then `something` is ready to use.

## Use it from a project

For example, I updated my `ytcasts` project to use this new library: <https://github.com/justinwoo/ytcasts/commit/300afc61a43f0f24d0839312b67e4ca8b1ef6079>

So in this case, I named my new tag `kefir`, and so my psc-package.json file is updated as such:

```diff
-  "set": "mandel",
+  "set": "kefir",
-  "source": "https://github.com/justinwoo/package-sets.git",
+  "source": "https://github.com/justinwoo/package-sets.git",
   "depends": [
+    "sunde",
      "node-he",
```

This is all documented on the psc-package README here: <https://github.com/purescript/psc-package#add-a-package-to-the-package-set>

And so, with this change I'm able to replace my repeated code and use this instead:

```diff
-runDownload (Url url) = makeAff \cb -> do
-  let cb' = cb <<< pure
-  process <- spawn "youtube-dl"
-             [ "-o"
-             , "downloads/%(title)s.%(ext)s"
-             , "-x"
-             , "--audio-format"
-             , "mp3"
-             , url
-             ]
-             $ defaultSpawnOptions { stdio = [Just Pipe] }
-  onError process $ toStandardError >>> Left >>> cb'
-  onExit process $ const (cb' $ Right "success?")
-  pure mempty
+runDownload (Url url) = do
+  result <- Sunde.spawn
+    "youtube-dl"
+    [ "-o"
+    , "downloads/%(title)s.%(ext)s"
+    , "-x"
+    , "--audio-format"
+    , "mp3"
+    , url
+    ]
+    $ CP.defaultSpawnOptions
+        { stdio = [Just CP.Pipe]
+        }
+  pure case result.exit of
+    CP.Normally 0 -> Right "success?"
+    _ -> Left result.stderr
```

And it's done. I then applied the same change to two other projects.

## Conclusion

Hopefully this has shown you that you can also make your own libraries and immediately put them to use in your own package sets. Even if there might be an existing package for something you want, if it doesn't give you what you want, you should feel free to make your own package and put it to use.

If nothing else, forking package-sets and applying some git knowledge to manage your own package set should carry you a long way.

## Links

* Psc-Package docs: https://github.com/purescript/psc-package#add-a-package-to-the-package-set

