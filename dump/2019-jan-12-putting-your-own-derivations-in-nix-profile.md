---
title: Putting your own derivations in Nix Profile
tags: NixOS
author: kimagure
slide: false
---
Users of Nix quite often run `nix-env -i [some-package]` and expect the build products to be available in their PATH, via some kind of configuration that Nix does when they source `$HOME/.nix-profile/etc/profile.d/nix.sh` in their shell configuration. But what is actually in their `~/.nix-profile`? And importantly, if a given package either doesn't build correctly from e.g. nixpkgs or build in a suitable manner, how does one get started with their own package? And importantly, what is the actual easiest way to install a set of packages where one can readily override these at will?

## Where do binaries live in Nix Profile?

If we inspect the file that we source in our e.g. `.bashrc`, we can see the anticlimactic answer:

```sh
# ...

    NIX_LINK=$HOME/.nix-profile

# ...

    export PATH="$NIX_LINK/bin:$__savedpath"
```

That's all. For the current generation of the Nix Profile (i.e. some product of Nix-Env run on some inputs), the binaries that are on your PATH can be found in `~/.nix-profile/bin`. If you list this directory, you'll see that each binary is then a symlink to a build product in `/nix/store` for a given derivation.

## What is in a Nix Store entry?

You can even look at the directory of where the outputs have been produced for a derivation. For example, I have my own derivation for Firefox:

```sh
> nix-store -q $(which firefox) --references
/nix/store/2rdblywdzfylrvn8c34gbabj9cn8ciqb-firefox-simple

# alias l='ls -lht --color'
> l $(nix-store -q $(which firefox) --references)
total 8.0K
dr-xr-xr-x 2 justin justin 4.0K Jan  1  1970 bin
dr-xr-xr-x 8 justin justin 4.0K Jan  1  1970 firefox
```

Seeing that the Nix Store entry for my custom derivation for Firefox has a `/bin` directory probably sets off some thoughts. Is there some kind of relationship between files in this Nix Store output and what is in Nix Profile?

And so we check:

```sh
> l ~/.nix-profile/firefox
lrwxrwxrwx 1 justin justin 66 Jan  1  1970 /home/justin/.nix-profile/firefox -> /nix/store/2rdblywdzfylrvn8c34gbabj9cn8ciqb-firefox-simple/firefox

> l ~/.nix-profile/bin/firefox
lrwxrwxrwx 1 justin justin 70 Jan  1  1970 /home/justin/.nix-profile/bin/firefox -> /nix/store/2rdblywdzfylrvn8c34gbabj9cn8ciqb-firefox-simple/bin/firefox
```

Nice! Turns out, installing things by Nix Env not only brings over binaries into the Nix Profile `/bin` directory, but also any other files in the output directory. This means that any `/etc`, `/share`, whatever directories are put into the output of a derivation also become available here. To put it shortly:

**A Nix Derivation is a description of how to produce an output directory, usually from some source input files.**

## Example (Non-NixOS) derivation

So personally, I haven't wanted to use the NixPkgs derivation for Firefox for various reasons. Since I use Ubuntu, Firefox will work out of the box from the tarball that they distribute. Instead of trying to manually juggle around some files to manage Firefox installations from the tarballs, I can instead write a Nix derivation:

```nix
# my derivation can optionally take a "pkgs" argument
# if not supplied, it will use the nixpkgs channel (see ~/.nix-channels)
{ pkgs ? import <nixpkgs> {} }:

# stdenv.mkDerivation is the most straightforward way to write a derivation
pkgs.stdenv.mkDerivation {
  # name of my derivation
  name = "firefox-simple";

  # the source input to use for my derivation. here, i just specify a tarball to be downloaded
  src = pkgs.fetchurl {
    url = "https://download-installer.cdn.mozilla.net/pub/firefox/releases/64.0.2/linux-x86_64/en-US/firefox-64.0.2.tar.bz2";
    
    # this SHA can be obtained by running `nix-prefetch-url [url]`
    sha256 = "19q9rsx7vh5w28fb3fyb7nb1gq98f29g3jlgnqi5cn6qkjyzm844";
  };

  # mkDerivation uses a setup shell script which has a concept of "phases"
  # see more from the nixpkgs docs and from nixpkgs pkgs/stdenv/generic/setup.sh
  phases = "unpackPhase installPhase";

  # bash script for how my derivation is built
  installPhase = ''
    # ensure my output directory exists with the right directories
    mkdir -p $out/bin
    mkdir -p $out/firefox

    # use tar to unpack the firefox contents
    # see the tar docs maybe for more info (or stack overflow)
    ${pkgs.gnutar}/bin/tar xf $src --strip 1 -C $out/firefox

    # symlink the firefox binary from the output to $out/bin/firefox
    ln -s $out/firefox/firefox $out/bin/firefox
  '';
}
```

This really is it. To install the package, I can now install it via two methods:

1) I save the expression/source in `firefox.nix` and run `nix-env -f firefox.nix -i`

2) I can make a new file called `packages.nix` and run `nix-env -f packages.nix -i`, where I declare a set/record:

```hs
{ pkgs ? import <nixpkgs> {} }:

{
  firefox = import ./firefox.nix { inherit pkgs; };
}
```

If you use this method, all packages in the set will be installed. This is by far the simplest way to source control Nix packages to be installed into your Nix Profile. No need for more complicated workflows or opaque tools.

Since I'm the only consumer of this derivation, I don't have to do any more work to make the binary work on other platforms or on NixOS. *I'll write about how utilities like patchelf and wrapProgram help you prepare programs that are well isolated and work on any environment in the future, but not here.*

## Conclusion

I hope this has given you some information on how to find out what is in your Nix Profile and how to install things into it from both existing NixPkgs derivations and your own.

Most of this information is available in various forms in the NixPkgs manual: <https://nixos.org/nixpkgs/manual/>

