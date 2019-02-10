---
title: Upgrade from Bower to Nix with Psc-Package2Nix
tags: purescript NixOS
author: kimagure
slide: false
---
Recently, our friends at LumiHQ released a major new version of React-Basic with a React-Basic-Starter. This was great, but there were two things I didn't like about this starter repo:

* This uses Bower, which is fairly slow and can make getting working dependencies a real pain in the ass.
* This uses Webpack with a hefty setup that I don't want to deal with.

Previously, I have written about upgrading from Bower to Psc-Package [here](https://qiita.com/kimagure/items/0d9354900d7a7dbd3864) and about how to efficiently manage package sets using Dhall [here](https://qiita.com/kimagure/items/c419ba740ac134a837a2). But I decided that this time, I would go the extra mile to upgrade this even further by using Psc-Package2Nix to make this work via Nix.

## Bower to Psc-Package

The `bower.json` in the project declares prelude, console, effect, and react-basic as dependencies. However, all of the other dependencies are either direct or transitive dependencies of react-basic, so we can just depend on react-basic.

Then we need a package set with the new version of react-basic, which is already prepared in Spacchetti: <https://github.com/justinwoo/spacchetti/releases/tag/101118>

So for starters, we can just make a `psc-package.json` file:

```json
{
  "name": "spacchetti-react-basic-starter",
  "set": "101118",
  "source": "https://github.com/justinwoo/spacchetti.git",
  "depends": [
    "react-basic"
  ]
}
```

## Psc-Package2Nix

Next, we need to at least get my PureScript tooling and Psc-Package2Nix. To do this, we can use my Easy-PureScript-Nix repo and pull in the specific inputs we need from this. To do this, we can use nixpkgs' `fetchFromGitHub`, with the sha256 hash grabbed by using `nix-prefetch-github`, and then build an "attribute set"/record that we will use to feed buildInputs into our default derivation:

```nix
let
  pkgs = import <nixpkgs> {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "ad581d3c63a8eb437c54a5afc08f2c1f6e7fbc9f";
    sha256 = "105i5w6na902z9kmcdg8gjm7d1l65hb1yb42zwpj4ffg7w90fhqj";
  });

  inputs = {
    inherit (easy-ps.inputs)
    purs
    psc-package-simple
    psc-package2nix;
  };

in pkgs.stdenv.mkDerivation {
  name = "spacchetti-react-basic-starter";

  buildInputs = builtins.attrValues inputs;
}
```

With this, we can start up a `nix-shell` that runs `psc-package2nix` by running `nix-shell --run 'psc-package2nix'`, which will generate a `packages.nix` file with derivations for our dependencies.

## Installing our dependencies

To install our dependencies, we can bring in the utils nix expression from the Psc-Package2Nix repo. This expression exposes a `mkDefaultShellHook` function we can use to get a `shellHook` for our expression that will copy our dependencies from the nix store into our project `.psc-package/` directory for Psc-Package to use. To import the utils expression, we can reference the file with `fetchurl` and use `nix-prefetch-url` to get the sha256 hash of this file into `install-deps.nix`:

```nix
let
  pkgs = import <nixpkgs> {};
  packages = import ./packages.nix {};
  packageDrvs = builtins.attrValues packages.inputs;
  pp2n-utils = import (pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/justinwoo/psc-package2nix/409aab26afa0784eb90440da33b1ad4d56aedb93/utils.nix";
    sha256 = "0rkqisfvpz5x8j2p0llv0yzgz5vnzy7fcfalp8nkymbshk8702gg";
  });

in pkgs.stdenv.mkDerivation {
  name = "install-deps";

  buildInputs = packageDrvs;

  shellHook = pp2n-utils.mkDefaultShellHook packages packageDrvs;
}
```

Now we can run a `nix-shell` to install our dependencies using `nix-shell install-deps.nix --run 'echo installation complete'`. Our shell hook will run first, then the echo will come afterwards.

The especially nice thing about using Nix for Psc-Package dependencies is that you will never have to download the same dependency again as long as it is in your store. This means that if you upgrade package sets or move to another project, only those packages that have not yet been downloaded will be downloaded. For me, this meant that I only had to download the new React-Basic dependency, since I had already downloaded the rest when working on other applications.

## Usage

This is essentially all of the setup we need, as then we can use `psc-package build` to build the project with the dependencies. To finish the conversion, we can go through some chores:

* We can delete bower.json
* Same with webpack.config.js
* Same with the scripts in package.json
* We can install parcel-bundler and do some other clean up such as adding the explicit `<script>` tag in `src/index.html` and importing from the PureScript compiler output.

Once we have done this, we can run `parcel src/index.html` and everything will work as expected, along with automatically working hot reloading for the React components. Here's a video of it working: <https://twitter.com/jusrin00/status/1060961458484326400>

## Bonus: Travis CI setup

With a Makefile that defines the steps we need (npm install, installing dependencies via Nix, building with purs and parcel), we can define a very short `.travis.yml` file for Travis integration:


```yml
sudo: required
dist: trusty
language: nix
script:
  - nix-shell --run make
```

## Conclusion

Hopefully this has shown you that you can get rid of Bower and use Nix with Psc-Package2Nix without too much hassle. I also hope that clicking through the links shows that there isn't too much involved in making the things that make up the individual parts to make this work, so there's not too much effort involved in trying to debug any of this. While we normally think of solutions presented by others as some kind of box of horrors, I hope in this case, you can comfortably look through all of the details and become empowered to make things work as you see fit.

## Links

* This repo: <https://github.com/justinwoo/spacchetti-react-basic-starter>
* Easy-PureScript-Nix: <https://github.com/justinwoo/easy-purescript-nix>
* Psc-Package2Nix: <https://github.com/justinwoo/psc-package2nix>

