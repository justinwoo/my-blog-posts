# My PureScript GitHub CI Setup in 2020

Recently, I had to fix some of my CI setups both on GitHub and off, and took some opportunity to start fixing some of the ways my tooling is set up. This will be a description of what I use to set up CI for my PureScript projects on GitHub making use of GitHub Actions.

## Structure

My project structure looks like this:

```
肉 ~/Code/my-purescript-ci-setup master $ tree -L 2
.
├── .github
│   └── workflows
├── .gitignore
├── psc-package.json
├── README.md
├── shell.nix
├── src
│   └── Main.purs
├── test
│   └── Main.purs
└── tests.bash
```

### `psc-package.json`

Yes, indeed, it is 2020 and I am still using Psc-Package quite happily. It has been and continues to be the most reliable tool I have to work with in PureScript other than the PureScript compiler itself.

```json
{
  "name": "my-purescript-ci-setup",
  "set": "psc-0.13.8",
  "source": "https://github.com/justinwoo/package-sets-jsonnet.git",
  "depends": [
    "console",
    "effect",
    "prelude"
  ]
}
```

You might ask why I don't use some other tool you have heard of or like, and usually the answer is just that I do not like them from having used them. But it's fine, you can use anything you want.

### `src/**/*.purs` and `test/**/*.purs`

This is where my sources live. I have thought about shoving `test/` into the `src/` tree, but for now I still keep it this way. My test file looks like so:

```purescript
module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  log "These are where my tests would go."
  log "Nowadays I will just FFI to Jest or any other tool of choice."
```

By default, Psc-Package doesn't make almost any assumptions about your project (which is quite nice). So be prepared to pass in additional compiler arguments to compile `test/` files. Relevant Psc-Package source code: https://github.com/purescript/psc-package/blob/24a696131bca63513a84d0a9b691f773c7b37069/app/Main.hs#L416-L422

### `shell.nix`

This is where the relevant tooling is provided to the environment, whether I am developing locally on my machine or running tests in CI. I mostly just need the PureScript compiler, Psc-Package, and NodeJS if I want to run tests.

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  easy-ps = import (
    pkgs.fetchFromGitHub {
      owner = "justinwoo";
      repo = "easy-purescript-nix";
      rev = "0ba91d9aa9f7421f6bfe4895677159a8a999bf20";
      sha256 = "1baq7mmd3vjas87f0gzlq83n2l1h3dlqajjqr7fgaazpa9xgzs7q";
    }
  ) {
    inherit pkgs;
  };
in
pkgs.mkShell {
  buildInputs = [
    easy-ps.purs
    easy-ps.psc-package
    pkgs.nodejs

    # if you need yarn
    # pkgs.yarn
  ];
}
```

### `tests.bash`

This is an executable script that will use nix-shell to run, so that I have my environment set with all the tooling I need. From here I call Psc-Package to install dependencies and do the build. Then, I can call node to run the main test module.

```bash
#!/usr/bin/env nix-shell
#!nix-shell shell.nix -i bash

psc-package install
psc-package build -- test/**/*.purs
node -e "require('./output/Test.Main').main()"
```

### `.github/workflows/tests.yml`

This GitHub Actions config uses the install-nix-action I wrote about last time (see links below) and then runs this executable script to run my tests.

```yml
name: tests

on:
  pull_request:
  push:
jobs:
  tests:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - uses: cachix/install-nix-action@v9
    - run: ./tests.bash
```

And that's it. The result takes less than a minute to run and can be seen on GitHub here: https://github.com/justinwoo/my-purescript-ci-setup/runs/721818959

## Conclusion

Hopefully this has given you some ideas on how you can make your own setups. If this helps you in some way, please say something in an appropriate way somewhere.

Also, chances are, you might be a corporate user who will copy this work to use in your project. That's fine. But if you would like to get help, please don't privately message me to get help unless you are going to pay me for my time and expertise. Thanks.

## Links

* This repo: https://github.com/justinwoo/my-purescript-ci-setup
* Relevant previous post about Nix on GitHub Actions: https://github.com/justinwoo/my-blog-posts/blob/master/posts/2020-05-25-nix-on-github-actions-in-2020.md
