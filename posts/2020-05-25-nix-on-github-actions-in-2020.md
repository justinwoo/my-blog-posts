# Nix on GitHub Actions in 2020

Chances are, you have now been exposed to some various projects on GitHub that use GitHub Actions, or you know of some various projects that use Travis or some other solutions that want to switch. It's understandable, given that many projects end up wasting hours, days, and weeks on CI setups that don't work, are unreliable, and/or are just too slow to provide meaningful feedback. Well, the last part isn't likely to be solved as long as you use someone else's basement toasters, but maybe we can more easily make it so that we can run the same build locally as we do in CI? To achieve this, I like to use Nix shell scripts.

For a while, installing Nix on GitHub Actions was kind of terrible. You could try running the installation script, but you would very readily run into segfaults and other nice failures running on GitHub's toasters. Thankfully, Domen from Cachix has been quite persevering in getting Nix to work on GitHub actions, so there is a working solution for that in `install-nix-action`: https://github.com/cachix/install-nix-action

## Example setup

Say you wanted to have rustup (https://rustup.rs/) in a Nix shell to work locally and to also have available on GitHub actions. We can set up a `shell.nix` like so:

```nix
{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  buildInputs =
    builtins.attrValues {
      inherit (pkgs) rustup;
    };
}
```

Then we can make an executable script `test` that will be interpreted by Nix shell.

```bash
#!/usr/bin/env nix-shell
#!nix-shell shell.nix -i bash

rustup update stable
cargo build
```

We can try running this locally:

```bash
$ ./test
info: syncing channel updates for 'stable-x86_64-unknown-linux-gnu'

  stable-x86_64-unknown-linux-gnu unchanged - rustc 1.43.1 (8d69840ab 2020-05-04)

    Finished dev [unoptimized + debuginfo] target(s) in 0.00s
```

Then we need to define a workflow that will use this in GitHub actions using `install-nix-action`. In my case I put it in `.github/workflows/test.yml`:

```yml
name: "Test"
on:
  pull_request:
  push:
jobs:
  tests:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v2
    - uses: cachix/install-nix-action@v8
    - run: ./test
```

This is the whole workflow definition. I do not try to put any more in here, because I do not want to deal with my CI having a completely different way of doing things than I can reproduce locally.

When this is pushed up, GitHub Actions will run.

### Result

You can look at the action that was run on GitHub here: https://github.com/justinwoo/rustup-test-gh-actions/runs/706966778?check_suite_focus=true

## Conclusion

I hope this has shown you how you can make use of Nix on GitHub Actions to make your life easier. I chose to write about this because there doesn't really seem to be many people writing about this at all, giving an illusion that there doesn't exist a solution that will work for most cases.

## Links

* install-nix-action https://github.com/cachix/install-nix-action
* this example https://github.com/justinwoo/rustup-test-gh-actions
