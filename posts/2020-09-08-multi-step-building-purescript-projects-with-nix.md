# Multi-step building PureScript projects with Nix

Recently, my friend James asked me to help him figure out how to build PureScript projects in parts, so that he could reduce the time spent downloading dependencies, building from those dependencies, and building between his various sub-projects. While this isn't too complicated, there are some complications forced on us by the PureScript compiler.

## What and Why

The idea seems simple: the compiler takes in a bunch of files and produces an output. For each file input, there is a produced module output consisting of information about the module and the generated JavaScript sources. An input might depend on other inputs, so then a graph must be constructed in any case. You might want to even go through the main build plan code of the compiler to see how it works yourself: <https://github.com/purescript/purescript/blob/58c101f44bc238dcff7a7b4bf8d2033aada68875/src/Language/PureScript/Make/BuildPlan.hs>

So unfortunately, going through this implementation and experimenting with its behavior lets us know of two realities:

* The file path of the input files is used for determining if an output corresponds to an input, not only the module name or file contents.

* The file timestamp of the input files and the corresponding timestamps of dependencies and their outputs are used in attempting to figure out if a file should be rebuilt.

We can see that this information is kept in various forms in the output, including this "cache" file that contains information on what file a given module originated from and its timestamp. Curiously, a hash of its contents is also kept here, but it is debatable whether or not this has any real purpose.

```js
// jq '.Main' result/output/cache-db.json

{
  "/nix/store/jixxv26z1qx5gc1cbw7jba9p0fdnbch6-src/Main.purs": [
    "1970-01-01T00:00:01Z",
    "7b12c16cdddfbef873fc179c51ce9de7ec0375da4509962fc0016cb9ef85c9ae561b6c088bc40903c8863e8ea847afeb4537dbb511469f83cdb090e04f618f1b"
  ]
}
```

Given this information, we know at least three things about how inputs and outputs need to be handled for most consistency:

* We must provide a static filepath to any given input file of a specific content, to ensure that any subsequent rebuilds do not trigger a recompilation of the input which has already been built.

* We must make sure timestamps of the input files are kept consistent, so that we do not end up with rebuilds for the same reason.

* We must copy over the output while also providing all of the same inputs if we are to build the output in a consistent way.

-----

An example where this goes wrong is when you work with large projects using the same output for different timestamps and contents. Naively rebuilding a project after checking out multiple different git branches can easily cause you some frustration with inconsistent builds and erroneously passing and failing builds. I have run into this problem with my own work before, with the solutions being to often wipe my outputs in CI (adding 2-5min of build time) and then to eventually go with a solution to build my project from the Nix store, preventing this problem at the expense of disk usage and additional build times. Please do not message me regarding how to reproduce this problem as I have no desire to further spend more of my own time to track down this issue.

-----

## How

As usual, my example will use Psc-Package-Nix, but you could use just about any solution you want. You could also choose to run whatever random package manager in an impure derivation, but I will leave that method only to madmen.

### generating purs-packages.nix

I need to get my dependencies in some way. In Psc-Package-Nix, I provide a tool to generate them easily by fetching all of them.

```nix
# shell.nix
{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import ./nix/easy-ps.nix { inherit pkgs; };
  ppn = import ./nix/psc-package-nix.nix { inherit pkgs; };

  generatePursPackages = import "${ppn}/nix/generate-purs-packages.nix" {
    inherit pkgs;
    packagesJson = ./packages.json;
    inputNames = (builtins.fromJSON (builtins.readFile ./psc-package.json)).depends;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs
    easy-ps.psc-package
    generatePursPackages
  ];

  shellHook = ''
    # this lets us generate purs-packages.nix
    mk-purs-packages-nix() {
      generate-purs-packages > nix/purs-packages.nix
      echo 'done'
    }
  '';
}
```

The output is a attribute set like so:

```nix
# nix/purs-packages.nix
{ pkgs ? import <nixpkgs> {} }:
{
  "console" = {
    name = "console";
    version = "v4.4.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-console.git";
      rev = "v4.4.0";
      sha256 = "1w9k2g242lvyv4npb5rqfbdq1ngh7s7v12zarxn8yxgq15palh3m";
    };
  };
  # ...
}
```

### The builds

So then we have the dependencies we need and know we can easily put our project sources into the nix store by referencing them by relative path. All that remains is the application of what I described above:

```nix
# nix/build-ps.nix
{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = import ./easy-ps.nix { inherit pkgs; };
  pursPackages = import ./purs-packages.nix { inherit pkgs; };

  # remember, purs takes quoted globs
  getQuotedSourceGlob = x: ''"${x.src}/src/**/*.purs"'';
  sourceGlobs = map getQuotedSourceGlob (builtins.attrValues pursPackages);
  sourceGlobsString = toString sourceGlobs;

  # this is the build output of just the dependencies
  pursPackagesOutput = pkgs.runCommand "test-psc-package-nix-output"
    {
      buildInputs = [ easy-ps.purs ];
    } ''
    mkdir $out
    cd $out
    purs compile ${sourceGlobsString}
  '';

  # then this is the build including the project sources
  projectSources = ''"${../src}/**/*.purs"'';
  projectOutput = pkgs.runCommand "test-psc-package-nix-output-project"
    {
      buildInputs = [ easy-ps.purs ];
    } ''
    mkdir $out
    cd $out
    # notice here that copying the previous output requires not preserving mode
    # because the output will be modified by the PureScript compiler
    cp -R --no-preserve=mode ${pursPackagesOutput}/output .
    purs compile ${sourceGlobsString} ${projectSources}
  '';
in
{
  inherit
    sourceGlobsString
    projectSources
    pursPackagesOutput
    projectOutput;
}
```

Then this can be built by referencing the specific attribute of the set.

```
$ nix-build ./nix/build-ps.nix -A pursPackagesOutput
/nix/store/q67dx313p7wfvynsd0h4hgrjmhw3cbk5-test-psc-package-nix-output

$ l result
lrwxrwxrwx 1 justin justin 71 Sep  2 01:58 result -> /nix/store/q67dx313p7wfvynsd0h4hgrjmhw3cbk5-test-psc-package-nix-output

$ ls result/output
cache-db.json         Data.Boolean          Data.Field            Data.Monoid.Disj            Data.Ord.Unsafe       Data.Symbol           Effect.Uncurried
Control.Applicative   Data.BooleanAlgebra   Data.Function         Data.Monoid.Dual            Data.Ring             Data.Unit             Effect.Unsafe
Control.Apply         Data.Bounded          Data.Functor          Data.Monoid.Endo            Data.Semigroup        Data.Void             Prelude
Control.Bind          Data.CommutativeRing  Data.HeytingAlgebra   Data.Monoid.Multiplicative  Data.Semigroup.First  Effect                Record.Unsafe
Control.Category      Data.DivisionRing     Data.Monoid           Data.NaturalTransformation  Data.Semigroup.Last   Effect.Class          Type.Data.Row
Control.Monad         Data.Eq               Data.Monoid.Additive  Data.Ord                    Data.Semiring         Effect.Class.Console  Type.Data.RowList
Control.Semigroupoid  Data.EuclideanRing    Data.Monoid.Conj      Data.Ordering               Data.Show             Effect.Console
```

## Conclusion

This kind of process can be likely applied to various other tools in a similar way, independent of PureScript.

I was going to write some more here, but I didn't come up with anything.

## Links

* This repo https://github.com/justinwoo/james-purescript-test
