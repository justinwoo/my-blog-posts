Previously, I wrote about how I made the Spago2Nix project and what was involved, but didn't go much into the details of how the implementation worked. This time, I'll write about how putting together a command line application with minimal setup doesn't need to involve too much work, and some of the most common tools you'll end up using to do so.

Writing a CLI typically involves three common tasks, so I will cover them in the sections below:

* Args
* Files
* Child processes

## Args

The main thing you need to know about handling args in Node is that there is a global `process` in Node, of which you can get its `argv` property as an array of strings. For example, given a `index.js` file that can be executed (`chmod +x`)

```js
#!/usr/bin/env node

console.log('hi, here is argv:');
console.log(process.argv);
```

When you execute this file, you will get a `process.argv` that has two args by default, which are

* the Node executable being run
* the path of the script that is being run (index.js)

Like so:

```bash
$ ./index.js
hi, here is argv:
[ '/some/path/nodejs/bin/node',
  '/home/justin/Code/spago2nix/index.js' ]

# we can also call node ourselves
$ node index.js
hi, here is argv:
[ '/some/path/nodejs/bin/node',
  '/home/justin/Code/spago2nix/index.js' ]
```

We can pass args to this to give it some more arguments:

```
$ node index.js hello world
hi, here is argv:
[ '/some/path/nodejs/bin/node',
  '/home/justin/Code/spago2nix/index.js',
  'hello',
  'world' ]
```

So knowing this, we can handle args quite easily in a PureScript program, where we first prepare a FFI file with the export:

```js
// src/Main.js
exports.argv = process.argv;
// btw this is the only FFI we need in our project
```

Then we can use this in our program:

```purs
# src/Main.purs
import Data.List (List, (:))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff

foreign import argv :: Array String

args :: List String
args = List.drop 2 $ List.fromFoldable argv

main :: Effect Unit
main = Aff.launchAff_ do
  case args of
    "generate" : List.Nil -> Generate.generate
    "install" : rest -> install rest
    "build" : rest -> build SpagoStyle rest
    "build-nix" : rest -> build NixStyle rest
    "help" : rest -> log help
    List.Nil -> log help
    _ -> do
      log $ "Unknown arguments: " <> List.intercalate " " args
```

As simple as that, we can handle arguments to our program by pattern matching on a `List String`.

If you have more various things to handle, you might consider having some data type for arguments that you serialize to, using some other library like purescript-optparse, or just anything more, but I think this does handle a good number of simple usages that people need.

## Files

Chances are, if you use PureScript, you use `Aff` to handle asynchronous effects. Accordingly, there is a [`node-fs-aff`](https://github.com/purescript-node/purescript-node-fs-aff) library that you can use to use Node's file system module to read and write files.

For example,

```purs
import Node.FS.Aff as FS

-- this ensures that I have a .spago2nix folder to work with,
-- handling errors (i.e. from an existing directory) with a no-op
ensureSetup = do
  FS.mkdir "./.spago2nix" <|> pure unit

-- this ensures that I have prefetched all of my dependencies,
-- then writes a UTF8 text file with my results
generate = do
  ensureSetup
  packages <- exitOnError spagoListPackages
  fetches <- toResult <$> parTraverse ensureFetchPackage packages
  case fetches of
    Left errors -> do
      error "errors from fetching packages:"
      traverse_ (error <<< show) errors
      exit 1
    Right xs -> do
      FS.writeTextFile UTF8 spagoPackagesNix (printResults xs)
      log $ "wrote " <> spagoPackagesNix
      exit 0
```

## Child processes

A CLI typically also needs to source information from other programs in various ways, most commonly by running other programs as child processes.

While working with the Node child process API via [`node-child-process`](https://github.com/purescript-node/purescript-node-child-process) isn't too much more than juggling a bunch of effects, I have made a small library that makes this as easy as using a record and some String values:

```purs
import Sunde as S

newtype DhallExpr = DhallExpr String

runDhallToJSON :: DhallExpr -> Aff String
runDhallToJSON (DhallExpr expr) = do
  result <- S.spawn
    { cmd: "dhall-to-json", args: [], stdin: Just expr }
    CP.defaultSpawnOptions
  case result.exit of
    CP.Normally 0 -> do
      pure result.stdout
    _ -> do
      error "error running dhall-to-json:"
      error $ show result.exit
      error result.stderr
      Aff.throwError $ Aff.error result.stderr
```

And so, we are able to easily run a program, use a string to feed into stdin, and get the result of running our program as a product of the exit code, stdout, and stderr. For more specific uses, you may want to deal with the Node child process API yourself either through the node-child-process library or by FFI, but most of the time, this will do it.

## Putting it all together

To actually consume this CLI, the simplest thing to do is to bundle the main module and allow for a shim to execute it. For example, first prepare an executable bin:

```js
// package.json
{
  "name": "your-name",
  // ...
  "bin": {
    // this will tell npm to symlink this bin when it installs
    "your-bin-name": "bin/index.js"
  },
  "scripts": {
    // this will let us bundle our app to output.js
    "mkbin": "spago bundle-app -t bin/output.js"
  },
  // ...
}

// bin/index.js
#!/usr/bin/env node
require('./output.js');
```

With this setup, we can simply check in the result of `npm run mkbin`. Running `npm link` will make `your-bin-name` available to run.

```bash
$ which your-bin-name
/home/your-user/.npm/bin/your-bin-name
```

## Conclusion

Hopefully this has shown you that making a CLI with PureScript on Node is quite easy, and doesn't require any fancy preparations. If you do want to try to use something more, you can, but there's no need to spend hours on trying to do any part of this if you have some ideas you want to test quickly.

## Links

* Spago2Nix <https://github.com/justinwoo/spago2nix>
