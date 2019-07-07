# Why Easy-PureScript-Nix

This is mostly a rant and should not be read by a large audience.

---

It's now been over half a year since I started my easy-purescript-nix project, where I provide nix expressions that actually work to provide various PureScript tooling for both non-NixOS and NixOS users. In this short post, I'll just elaborate a bit on some various reasons why I made this project, and why it is now used in nixpkgs for the default all-packages "purescript" derivation.

I realize this post will make me somewhat of a pariah in the Nix community, but I'm also tired of people claiming "Easy-PureScript-Nix is bad" without any consideration of the details. And really, patching santa's little ELFs with locations to dynamic dependencies is one of the most important and good parts of Nix, and people should use it more.

## What Nixpkgs used to have, what I dislike about haskellPackages, and trivia

Nixpkgs used to provide a "purescript" derivation that did the following:

* try to pin some version of GHC
* ...which comes with some set of libraries at specific versions.
* try to use this GHC and associated haskell packages to build the PureScript compiler.

So what is wrong with this? Let's remember that the PureScript compiler is a regular Stack project, where none of the main contributors are Nix users. More importantly, this means that the compiler project has basically two things we should keep in mind:

* as a Stack project, it uses a Stackage lts package set.
* as a Stack project with need for dependencies at specific versions, it uses extra-deps.

This means that it will build with some GHC version that is defined in the Stack project configurations. Let's review what we have for the haskellPackages "purescript" derivation from Nixpkgs `pkgs/development/haskell-modules/hackage-packages.nix`:

```nix
  "purescript" = callPackage
    ({ mkDerivation, aeson, aeson-better-errors, ansi-terminal
     # ...
     , warp, websockets
     }:
     mkDerivation {
       pname = "purescript";
       version = "0.12.4";
       sha256 = "1lkicclh9gm3lwgi2vl7qqa2vzf763rw06m79mr1j291z4h2ym76";
       isLibrary = true;
       isExecutable = true;
       libraryHaskellDepends = [...];
       executableHaskellDepends = [...];
       testHaskellDepends = [...];
       testToolDepends = [ hspec-discover ];
       doCheck = false;
       description = "PureScript Programming Language Compiler";
       license = stdenv.lib.licenses.bsd3;
       hydraPlatforms = stdenv.lib.platforms.none;
       broken = true; ## NOTE: see how this package has been marked broken b514a6d35191 (Peter Simons 2019-03-16 17:03:43 +0100 182937)
     }) {};
```

Unsurprisingly, this means that the "purescript" derivation often breaks, as none of the Stack project configurations are used, and packages in haskellPackages change often (for good reason!). This means that often, the PureScript compiler had a lot of various issues filed from NixOS users, often asking for LTS updates to fix the Nixpkgs "purescript" derivation. Can we acknowledge how weird this sounds? "Hi, I don't use any dependencies you have declared in your project, but I have problems using random other dependencies. Please fix." This also means that any breaking changes that can be compiled between versions of a dependency can sneak in, such as encoding changes in Aeson.

This is why in nixpkgs, the "purescript" derivation used to pin to using ghc843.haskellPackages. Because it "worked".

## What Easy-PureScript-Nix does

Rather than to try to build the compiler in some manner that will still lead to people having multiple non-official versions of the compiler, Easy-PureScript-Nix takes the approach that we can simply actually use the exact same compiler build that everyone else has via GitHub releases, so we don't have to deal with potential incompatibility problems. This is accomplished by patching the binary and providing the dynamically linked libraries required:

```nix
{ pkgs ? import <nixpkgs> {} }:

let
  dynamic-linker = pkgs.stdenv.cc.bintools.dynamicLinker;

  # darwin is terrible so we can't do anything there
  patchelf = libPath: if pkgs.stdenv.isDarwin
    then ""
    else ''
          chmod u+w $PURS

          # replace the linker, and then set the rpath with libs that we need
          patchelf --interpreter ${dynamic-linker} --set-rpath ${libPath} $PURS

          chmod u-w $PURS
        '';

in pkgs.stdenv.mkDerivation rec {
  name = "purs-simple";

  version = "v0.12.5";

  src = if pkgs.stdenv.isDarwin
    then pkgs.fetchurl {
      url = "https://github.com/purescript/purescript/releases/download/v0.12.5/macos.tar.gz";
      sha256 = "15j9lkrl15dicx37bmh0199b3qdixig7w24wvdzi20jqbacz8nkn";
    }
    else pkgs.fetchurl {
      url = "https://github.com/purescript/purescript/releases/download/v0.12.5/linux64.tar.gz";
      sha256 = "07dva5gxq77g787krscv4dsz5088fzkvpmm9fwxw9a59jszzs7kq";
    };

  # dependencies that we need
  buildInputs = [ pkgs.zlib pkgs.gmp pkgs.ncurses5 ];

  # call hte utility function to make a library path from what we need
  libPath = pkgs.lib.makeLibraryPath buildInputs;

  dontStrip = true;

  installPhase = ''
    mkdir -p $out/bin
    PURS="$out/bin/purs"

    install -D -m555 -T purs $PURS
    ${patchelf libPath}

    mkdir -p $out/etc/bash_completion.d/
    $PURS --bash-completion-script $PURS > $out/etc/bash_completion.d/purs-completion.bash
  '';
}
```
This is it. The added bonus here is that installing this is so fast that you don't even need a binary cache.

## This is the default in Nixpkgs now for a reason

It's not hard to imagine that this had been an issue for years, with NixOS users and maintainers constantly filing issues in the PureScript compiler project to update dependencies to newer LTSes and non-pinned extra-deps so that the "purescript" derivation could evaluate. As you can imagine, many complaints never had a corresponding PR, because actually upgrading LTSes is quite a lot of work sometimes (surprise!). After the nth time seeing this issue come up and people I know not even trying PureScript because of this blocker, I decided to do the unthinkable: take matters into my own hands. I went and put the contents of Easy-PureScript-Nix into nixpkgs and replaced the attribute in all-packages.

And given the history of this package and the various other contexts (n.b. endless drama hell), my changes were merged quickly thanks to Graham and Tobi.

## If you're going to insist on doing some other shit, at least do it right

There seem to be some various developments in actually generating Nix expressions from Stack project files, as there should be. I will never want to use these, but someone who insists on not using patchELF might find them useful.

## P.S.

Arian has written on Twitter about some of the disappointments of the above here: <https://twitter.com/ProgrammerDude/status/1123220531044548612>
