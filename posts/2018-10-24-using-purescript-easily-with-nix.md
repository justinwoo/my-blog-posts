For a while, I've been wanting to come up with some kind of way to download and prepare programs I need to work with my various PureScript/PS-related Dhall projects, but could not really come up with a clear solution. While one popular method is to try to use Docker containers for everything, it incurs a lot of costs both on initialization and repeat initializations, and while many use npm to manage all of their dependencies, many times I am both not running on environments with npm installed nor do I want to use npm to manage installations. So, I ended up with Nix.

By the way, I use Ubuntu because I am pretty lazy. So most of this applies to Nix on Ubuntu (e.g. Travis, Azure CI), not NixOS.

## Desired characteristics

In this exercise, there were some essential characteristics I wanted:

* I should be able to use binaries from GitHub releases of projects, not rebuild the projects.
* I should be able to easily reuse my defined derivations from projects remotely.
* I should be able to use my derivations from CI readily.
* I should be able to use my derivations to install globally to my own system readily.

Beware that what this blog post contains are going to be very unpopular ways of getting things done with Nix. Expect nothing to work on NixOS. But it does work on Travis and Azure CI, as we'll see later on.

## Getting `purs`

First, I needed to get the PureScript compiler up and running. In the past, I've had a hell of a time trying to get the PureScript compiler to be installed through Nix with all of the failing derivations, and had enough of it. I wanted to instead download the damn binary from the tarball that's on the GitHub release page and put it to use, since that's exactly what I do now if I'm not trying to develop something on the compiler itself. And so, after an hour of going through other people's derivations and reading some code from the Nix standard library, I figured out something. *By the way, did I mention that dynamic typing makes this much harder to understand than it needs to be?*

```nix
# this expression can be passed in `pkgs`, but will use `import <nixpkgs> {}` by default.
{ pkgs ? import <nixpkgs> {} }:

# make a derivation, using a 'recursive' record (i.e. attributes may refer to each other)
pkgs.stdenv.mkDerivation rec {

  # i don't believe in using version numbers as part of the name
  name = "purs-simple";

  # version number i'm going to interpolate
  version = "v0.12.0";

  # set the source directory contents to what i fetch from this url
  src = pkgs.fetchurl {

    # url for my binary-containing tarball
    url = "https://github.com/purescript/purescript/releases/download/${version}/linux64.tar.gz";

    # sha256 for my tarball file, which can be obtained by using
    # `nix-prefetch-url https://github.com/purescript/purescript/releases/download/v0.12.0/linux64.tar.gz`
    sha256 = "1wf7n5y8qsa0s2p0nb5q81ck6ajfyp9ijr72bf6j6bhc6pcpgmyc";
  };

  # btw, did i mention the build process will magically unzip the tarball for you?
  # check out pkgs/stdenv/generic/setup.sh in nixpkgs for more information:
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/stdenv/generic/setup.sh

  # installation phase! it's time to copy over stuff into our output folder
  installPhase = ''
    # make the bin dir
    mkdir -p $out/bin

    # use install to copy of the purs binary
    # btw this is available because src has the contents of the purescript/ dir in the tarball
    install -D -m555 -T purs $out/bin/purs
  '';
}
```

*We'll come back to the phases a little later when we have to unpack some tarballs that don't have directories in them (yes, that's a special case).*

And there we have it, we have a derivation that works for getting the PureScript compiler binary. We can even use it to install the PureScript compiler: `nix-env -f purs.nix -i purs-simple`

The steps are essentially the same for setting up `psc-package`, `dhall`, and `dhall-json`.


## Getting `purp` and `spacchetti`

In the case that the tarballs don't unpack with a directory, the generic build will error: ["unpacker appears to have produced no directories"](https://github.com/NixOS/nixpkgs/blob/2eb372d59dd67e7eabe9f35472c10716b4cb38f7/pkgs/stdenv/generic/setup.sh#L892-L895).

Since I'm not going to bother with trying to change how `purp` and `spacchetti` are released, I will instead just opt to override the unpacking phase of the generic build:

```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  name = "spacchetti";
  version = "0.3.0.0";

  src = pkgs.fetchurl {
    url = "https://github.com/justinwoo/spacchetti-cli/releases/download/${version}/linux.tar.gz";
    sha256 = "0rdqh7y1yb3wyjaws2alb278h6izifa9adlqzk6sp5yvdjkai7kx";
  };

  ## unpack this tarball myself
  unpackPhase = ''
    mkdir -p $out/bin
    tar xf $src -C $out/bin
  '';

  ## don't run install, since the binary is already there
  dontInstall = true;
}
```

So with this, we have the binaries ready for use without too much extra work. 

## `default.nix`

Now we can get to making a `default.nix`:

```nix
let
  # using the normal nixpkgs
  pkgs = import <nixpkgs> {};

  # let's prepare a record/"set" of these derivations
  inputs = {
    purs = import ./purs.nix {};
    psc-package-simple = import ./psc-package-simple.nix {};
    purp = import ./purp.nix {};

    dhall-simple = import ./dhall-simple.nix {};
    dhall-json-simple = import ./dhall-json-simple.nix {};
    spacchetti-cli = import ./spacchetti-cli.nix {};
  };

  # prepare the buildInputs list for situations where we would want to use it
  # using attrValues to pull out the derivation values from the set
  buildInputs = builtins.attrValues inputs;
in {
  # let's export inputs for consumers to use
  inputs = inputs;

  # same with buildInputs
  buildInputs = buildInputs;

  # let's prepare the shell for use elsewhere
  shell = pkgs.stdenv.mkDerivation {
    name = "easy-purescript-nix-shell";
    src = ./.;
    buildInputs = buildInputs;
  };
}
```

Then for our shell, we can import `default.nix` and grab the shell derivation from the `shell` attribute:

```nix
(import ./default.nix).shell
```

Now we can start using this in shells, just by calling `nix-shell` from the containing directory.

## Using this from downstream projects

To use this from downstream projects, you just need to define some nix file. For example, `shell.nix` in the root of some other project:

```nix
let
  pkgs = import <nixpkgs> {};

  # import the remote nix expression
  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";

    # at this git revision:
    rev = "43b18de";

    # this sha can be obtained by using
    # nix-prefetch-git justinwoo easy-purescript-nix --rev 43b18de
    # (without --rev for latest master)
    sha256 = "0c36pxafmlyq643kvyq61rw8z3h5dbg5gjb6mi7rxb5bsasqwxp8";
  });
in pkgs.stdenv.mkDerivation {
  name = "easy-ps-test";
  src = ./.;

  buildInputs = easy-ps.buildInputs;
}
```

Then you can `nix-shell` away from your downstream!

## Using this from CI

I now have the [Spacchetti](https://github.com/justinwoo/spacchetti) project using this setup and running this on both Travis and Azure CI. First, this is the Make target I'm using:

```Makefile
ci: setup-only
	psc-package verify
```

Then my Travis setup only has to be two lines:

```yaml
language: nix
script: nix-shell easy-purescript.nix --run 'make ci'
```

Azure CI doesn't seem to come with Nix already installed, but that's no problem:

```yaml
pool:
  vmImage: 'Ubuntu 16.04'

steps:
- script: |
    curl https://nixos.org/nix/install | sh
    . /home/vsts/.nix-profile/etc/profile.d/nix.sh
    nix-shell easy-purescript.nix --run 'make ci'
  displayName: 'Install deps and run'
```

Then we're done! Check out the running examples from the badges on <https://github.com/justinwoo/spacchetti>. You can also see another example in <https://github.com/justinwoo/vidtracker>

## Conclusion

Hopefully this has shown you an easy way to work with PureScript and related tools using Nix on non-NixOS. In the end, using Nix in this fashion is fairly unpopular, but is a good way to quickly get results to use in various environments that you need to run on. You might even start using a nix file to install tools to your environment in your dotfiles:

```nix
let
  easy-ps = import ./easy-ps.nix;
in {
  inherit (easy-ps.inputs)
    purs
    psc-package-simple

    purp
    dhall-simple
    dhall-json-simple
    spacchetti-cli;
}
```

## Links

* This project: <https://github.com/justinwoo/easy-purescript-nix>
* genericBuild, the thing you will be fighting a lot: <https://github.com/NixOS/nixpkgs/blob/516681c8726018fecd0e31ae755cb9a9c01a75e5/pkgs/stdenv/generic/setup.sh#L1242>