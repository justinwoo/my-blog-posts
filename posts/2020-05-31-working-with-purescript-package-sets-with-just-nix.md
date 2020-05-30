# Working with PureScript package sets with just Nix

Recently, I started to think about how I would like to work with PureScript projects by mostly only using the PureScript compiler and Nix. While Psc-Package has served me well for a couple of years now, it ends up being yet another dependency with its own slew of behaviors, notably being its usage of git to fetch files from scratch.

I started asking around about how I would solve for transitive dependencies with Nix expressions, and if implementing this in pure Nix would be slow to evaluate. Thankfully, puck from freenode suggested I could just use the Nix builtin `genericClosure` function, so then I no longer needed to think about implementing it myself, and just got to work doing the implementation myself.

## What are package sets again?

Package sets used by Psc-Package are just that: sets of packages. These sets are represented as an object of objects in JSON with the following information per entry:

* name -- the name of the package that is stored in the key of the value in the object.
* repo -- the git url that a package can be found by
* version -- the version of the package that should be used in this package set
* dependencies -- a list of packages that this package depends on

Note that dependencies isn't an exhaustive list, which means that when we collect all of the packages we need to install in our project, we not only need the directly defined dependencies but all of the transitive ones also.

## How do we solve for transitive dependencies?

To do this, we can make use of the `genericClosure` builtin. Like many things in Nix, this is really undocumented other than just in the source code, so you can look at the source code here: https://github.com/NixOS/nix/blob/f60ce4fa207a210e23a1142d3a8ead611526e6e1/src/libexpr/primops.cc#L365-L426

Going through the implementation and the comments here, we can guess that the builtin is used by preparing a "startSet" which contains initial values to unfold, of which individual set elements are passed to an "operator" function which will in turn produce a new list of set items that should be included in the result.

## Solving the set

The implementation then can be done in a fairly straightforward way. In my project, I first started by simply vendoring in an existing package set to work with by default, which can be replaced by a path to a preferred package set. Nix lets you import JSON as Nix values from read in files:

```nix
{ pkgs ? import <nixpkgs> {}
, packagesJson ? ../vendor/packages.json
, inputNames # List PackageName e.g. [ "simple-json" ]
}:
let
  json = builtins.fromJSON (builtins.readFile packagesJson);

  # ... continues below
```

With this, we have a `json` value which is an attribute set of packages from packages.json. Remember that packages.json looks like this:

```json
"simple-json": {
  "dependencies": [
    "arrays",
    // ...
    "typelevel-prelude",
    "variant"
  ],
  "repo": "https://github.com/justinwoo/purescript-simple-json.git",
  "version": "v7.0.0"
},
```

So then we can prepare this information in the way `genericClosure` needs to work with it to solve for the transitive dependencies:

```nix
  # for each package, we need to make an entry that has the name and its dependencies as package
  getJsonPkgFromName = name:
    {
      key = name;
      deps =
        let
          depNames = (builtins.getAttr name json).dependencies;
        in map getJsonPkgFromName depNames;
    };

  # we use the function above to get a list of the packages in the structure we need
  inputPkgs = map getJsonPkgFromName inputNames;

  # then we use generic closure to solve for the inputs and their dependencies
  # we start with input provided to the expression from the top
  closure = builtins.genericClosure
    {
      startSet = inputPkgs;
      operator = x: [ x ] ++ x.deps;
    };
```

And this is the core of what we need. We can look at the result by making a test shell that uses this expression:

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  solvedSet = import ../nix/solve-set.nix {
    inherit pkgs;
    packagesJson = ../vendor/packages.json;
    inputNames = [ "simple-json" ];
  };
in
pkgs.mkShell {
  shellHook = ''
    echo "Solved package set: ${toString solvedSet.pkgNames}"
  '';
}
```

Opening a nix-shell with this expression will greet us with this message:

```
Solved package set: simple-json arrays exceptions foreign foreign-object globals nullable prelude record typelevel-prelude variant bifunctors control foldable-traversable maybe nonempty partial st tailrec tuples unfoldable unsafe-coerce effect either functions identity integers lists strings transformers gen proxy type-equality enums newtype orders invariant refs distributive math lazy
```

These are the dependencies we need to get in order to build our project.

## Generating Nix expressions

So we still need to fetch these dependencies to be able to use them to compile. However, our package set JSON does not come with sha256 we need to properly fetch packages in Nix. This means that we need to generate a script we can run to generate expressions for these packages with hashes, and then use these expressions to build.

First, we should go back up to where we solved for the package names and prepare information we need to work with. We can prepare a JSON file that we will feed into a script to fetch hashes like so:

```nix
  getPkgSpec = { key, ... }:
    let
      x = builtins.getAttr key json;
    in { name = key; value = { inherit (x) repo version; }; };
  pkgSpecs = builtins.listToAttrs (map getPkgSpec closure);
  pkgSpecsJSON = pkgs.writeTextFile {
    name = "package-specs.json";
    text = builtins.toJSON pkgSpecs;
  };
```

I prepared a Node.js program for the fetching of hashes in parallel, but you could use just about anything you want. My core logic is in this function which calls nix-prefetch-git:

```js
async function getSHASpec(name) {
  const pkgSpec = packageSpecsJSON[name];
  const repo = pkgSpec.repo;
  const version = pkgSpec.version;

  const { stdout, stderr } = await exec(
    `nix-prefetch-git ${repo} --rev ${version}`
  );
  const result = JSON.parse(stdout);
  return { name, repo, version, sha256: result.sha256 };
}
```

The script is then called with the package specs JSON file I prepared above.

```nix
{ pkgs ? import <nixpkgs> {}
, packagesJson ? ../vendor/packages.json
, inputNames # List PackageName e.g. [ "simple-json" ]
}:
let
  solvedSet = import ./solve-set.nix {
    inherit pkgs packagesJson inputNames;
  };

  generatePursPackages = pkgs.writeShellScriptBin "generate-purs-packages" ''
    node ${../src/generate-purs-packages-nix.js} ${solvedSet.pkgSpecsJSON}
  '';
in
pkgs.runCommand "generate-purs-packages" {
  src = generatePursPackages;
  buildInputs = [
    pkgs.makeWrapper
  ];
} ''
  mkdir -p $out/bin
  target=$out/bin/generate-purs-packages
  cp $src/bin/generate-purs-packages $target
  chmod +x $target
  wrapProgram $target --prefix PATH : ${pkgs.lib.makeBinPath [
  pkgs.nodejs
  pkgs.nix-prefetch-git
]}''
```

So this output wrapped program has Node and nix-prefetch-git available in PATH to use. Then we can prepare a shell which will make use of this generate script:

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  generatePursPackages = import ../nix/generate-purs-packages.nix {
    inherit pkgs;
    packagesJson = ../vendor/packages.json;
    inputNames = [ "simple-json" ];
  };
in
pkgs.mkShell {
  buildInputs = [
    generatePursPackages
  ];
}
```

So we can generate a `purs-packages.nix` file by running `generate-purs-packages > purs-packages.nix` in this Nix shell. The result will look pretty familiar:

```nix
{ pkgs ? import <nixpkgs> {} }:

{
  "arrays" = {
    name = "arrays";
    version = "v5.3.1";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-arrays.git";
      rev = "v5.3.1";
      sha256 = "1z8i4mr30mjsvmw743g0m1yxfgqa9rhbgq7jq3955mg7j80j5r7w";
    };
  };
  "bifunctors" = {
    name = "bifunctors";
    version = "v4.0.0";
    src = pkgs.fetchgit {
      url = "https://github.com/purescript/purescript-bifunctors.git";
      rev = "v4.0.0";
      sha256 = "1bdra5fbkraglqrrm484vw8h0wwk48kzkn586v4y7fg106q1q386";
    };
  };
  # ...
```

## Building with these dependencies

There isn't too much left to do now with these generated results than to consume them in any way you want. We can use these to create quoted source globs to feed into PureScript:

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  pursPackages = import ./purs-packages.nix { inherit pkgs; };

  getQuotedSourceGlob = x: ''"${x.src}/src/**/*.purs"'';
  sourceGlobs = map getQuotedSourceGlob (builtins.attrValues pursPackages);
in
pkgs.runCommand "test-psc-package-nix-output" {
  buildInputs = [ pkgs.purescript ];
} ''
  mkdir $out
  cd $out
  purs compile ${toString sourceGlobs}
''
```

## Conclusion

Hopefully this has shown you how Psc-Package-Nix works in being able to solve for dependencies from packages.json and how you can use it to generate expressions to use in your own projects in however you like.

And please, don't ask me personally for help with PureScript and Nix. I do not publish these just to have people treat me as a free consulting service. Try asking in some forum or chatroom that actually aims to try to help people, and don't ping me in them, especially if I do not participate in them. If you are using this for work, please consider paying me for help.

Otherwise, if you like this post, please say something about it in an appropriate way. Thanks.

## Links

* This project: https://github.com/justinwoo/psc-package-nix/
