Recently, some people have asked me how to build PureScript projects with Nix using Psc-Package2Nix. Admittedly, I haven't seriously tried to do this before with PureScript, because I use Nix primarily to prepare and build tooling and set up my environment. However, doing this with information generated from Psc-Package2Nix is fairly straightforward, but does involve writing some Nix.

## From the top

So first, we can start from a project with the regular setup:

```
psc-package.json
src/
  Main.purs
```

As usual, `psc-package.json` will contain normal Psc-Package information:

```json
{
  "name": "purescript-nix-output",
  "set": "psc-0.12.3",
  "source": "https://github.com/purescript/package-sets.git",
  "depends": [
    "console",
    "prelude"
  ]
}
```

Then we can use [Psc-Package2Nix](https://github.com/justinwoo/psc-package2nix) to generate a Nix expression with our packages.

```bash
> psc-package2nix # or pp2n psc-package2nix
wrote packages.nix
```

## `packages.nix`

The `packages.nix` output looks like this:

```nix
{ pkgs ? import <nixpkgs> {} }:

let
  inputs = {

    "console" = pkgs.stdenv.mkDerivation {
      name = "console";
      version = "v4.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-console.git";
        rev = "v4.2.0";
        sha256 = "1b2nykdq1dzaqyra5pi8cvvz4dsbbkg57a2c88yi931ynm19nnw6";
      };
      phases = "installPhase";
      installPhase = "ln -s $src $out";
    };
    
    # other packages...
  };

in {
  inherit inputs;

  set = "psc-0.12.3";
  source = "https://github.com/purescript/package-sets.git";
}
```

So we can see that the generated expression has a set ("record") named `inputs`, where each input contains the dependency that was cloned.

## Supplying inputs to PureScript

The PureScript compiler works by simply being passed a bunch of globs for where to find files. This means that if we provide some prefixed globs of `"src/**/*.purs"`, we can feed the inputs into the compiler to build.

Knowing this, we know we can prepare globs by mapping a function to produce the globs from `inputs`:

```nix
{ pkgs ? import <nixpkgs> {} }:

let
  packages = import ./packages.nix { inherit pkgs; };

  getGlob = pkg: ''"${pkg}/src/**/*.purs"'';
  globs = with builtins; toString (map getGlob (attrValues packages.inputs));
```

Then we can later call `purs` with these globs.

## Writing the whole derivation

So now we know how to prepare the globs, so all that is left is to provide the PureScript compiler and write our project derivation. The PureScript compiler can be obtained from my [Easy-PureScript-Nix](https://github.com/justinwoo/easy-purescript-nix) project. Then we can call the PureScript compiler in the derivation's install phase and move the output produced into where it needs to go. And so:

```nix
{ pkgs ? import <nixpkgs> {} }:

let
  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "d383972c82620a712ead4033db14110497bc2c9c";
    sha256 = "0hj85y3k0awd21j5s82hkskzp4nlzhi0qs4npnv172kaac03x8ms";
  });

  packages = import ./packages.nix { inherit pkgs; };

  getGlob = pkg: ''"${pkg}/src/**/*.purs"'';
  globs = with builtins; toString (map getGlob (attrValues packages.inputs));

in pkgs.stdenv.mkDerivation {
  name = "purescript-nix-output-demo";
  src = ./.;

  installPhase = ''
    rm -rf output # ensure we dont use stale output
    ${easy-ps.purs}/bin/purs compile ${globs} "src/**/*.purs"
    mkdir -p $out
    mv output $out
  '';
}
```

And this is it. When we run `nix-build`, we will produce a `result` symlink where we can see the output, and we can even run the output directly using node:

```
> node -e "require('./result/output/Main').main()"
Hello World
```

## Conclusion

Hopefully this has shown you that building the PureScript part of your project is fairly straightforward if you use a Psc-Package configuration file and use Psc-Package2Nix.

## Links

* This demo <https://github.com/justinwoo/purescript-nix-output>
* Easy-PureScript-Nix <https://github.com/justinwoo/easy-purescript-nix>
* Psc-Package2Nix <https://github.com/justinwoo/psc-package2nix>
