Recently, I needed to override some version bounds for building a Haskell project, where I needed a dependency with the most typical Haskell problem ever: The upper bound was too restrictive.

## Preparing a GHC derivation to use in your build

Typically, preparing packages you want to be available in ghc-pkg is just a matter of using ghc.withPackages:

```bash
> nix-shell -p 'ghc.withPackages(pkgs: with pkgs; [unliftio])'
# ...

> ghc-pkg list | grep -e '/' -e unlift
/nix/store/h58x991z9n8xwq4zkzf4dfgpb6cs84c2-ghc-8.6.3-with-packages/lib/ghc-8.6.3/package.conf.d
    unliftio-0.2.10
    unliftio-core-0.1.2.0
```

## Mismatched version ranges

However, we'll find that some packages don't work, such as async-pool. We can try running it:

```bash
> nix-shell -p 'ghc.withPackages(pkgs: with pkgs; [async-pool])'
# ...
Setup: Encountered missing dependencies:
base >=3 && <4.12

builder for '/nix/store/7agl5jrmaxp7dzrmvfjcn08vdlk8lmyn-async-pool-0.9.0.2.drv' failed with exit code 1
cannot build derivation '/nix/store/7kx0vhvf74y7jliy467dxnf0kxhgx4ar-ghc-8.6.3-with-packages.drv': 1 dependencies couldn't be built
error: build of '/nix/store/7kx0vhvf74y7jliy467dxnf0kxhgx4ar-ghc-8.6.3-with-packages.drv' failed
```

Missing `base` does not make any sense, because this implies that the GHC that we have is junk and doesn't contain basic boot libraries that GHC is distributed with. But we can see how this fails also in Hydra with the same error: <https://hydra.nixos.org/build/86369536>. This is why there's a joke: "if it starts to build locally, you already know it's broken on Hydra".

However, the error *is* right, in that we don't have base at <4.12. See what we get with an empty package list:

```bash
> nix-shell -p 'ghc.withPackages(pkgs: with pkgs; [])' --run 'ghc-pkg list' | grep base
    base-4.12.0.0
```

And we can check in Hackage to see that there really isn't a newer package with fixed version ranges (as of 2019 Mar 16): <http://hackage.haskell.org/package/async-pool>

![](https://i.imgur.com/8v1dOkV.png)

## Breaking free from version bounds hell

In nixpkgs, there is a lib module we can use for Haskell packages in `pkgs.haskell.lib`, in which we can find an appropriate function for overriding cabal derivations:

```nix
  /* doJailbreak enables the removal of version bounds from the cabal
     file. You may want to avoid this function.

     This is useful when a package reports that it can not be built
     due to version mismatches. In some cases, removing the version
     bounds entirely is an easy way to make a package build, but at
     the risk of breaking software in non-obvious ways now or in the
     future.

     Instead of jailbreaking, you can patch the cabal file.
   */
  doJailbreak = drv: overrideCabal drv (drv: { jailbreak = true; });
```

However, simply using this function leads us to suffer slow "testing" of our overridden module, which we generally do not care about. Unless you absolutely love running your dependencies' tests, but this is Haskell, and I don't have that kind of patience.

But we can use the same idea to override multiple properties, which in our case we only need to also override `doCheck`. So we can prepare a derivation for GHC where version bounds are thrown away for `async-pool`:

```bash
> nix-shell -p 'ghc.withPackages(pkgs: [ (haskell.lib.overrideCabal pkgs.async-pool (old: { jailbreak = true; doCheck = false; })) ])'
> ghc-pkg list | grep async-pool
    async-pool-0.9.0.2
```

Cool! Normally we don't need to create a bash command from hell, but at least now we know it will work in an expression.

## Using this in some derivation

I recently needed this to add some parallel fetching to Psc-Package2Nix, where my Haskell program replaced the Perl program that was there before. The derivation goes roughly like so:

```nix
{ pkgs ? import <nixpkgs> {} }:

let
  # our GHC with async-pool that we know works already
  ghc = pkgs.ghc.withPackages (x: [
    (pkgs.haskell.lib.overrideCabal x.async-pool (old: {
      jailbreak = true;
      doCheck = false;
    }))
  ]);

in pkgs.stdenv.mkDerivation {
  name = "psc-package2nix";

  src = ./.;

  buildInputs = [
    pkgs.makeWrapper
    # i used buildInputs to make GHC available
    ghc
  ];

  installPhase = ''
    mkdir -p $out/bin
    
    # ...

    # call GHC with various options to turn on threading
    # you know GHC is 90s software by reading this line
    ghc -threaded -rtsopts -with-rtsopts="-N" -o pp2n pp2n.hs

    install -D -m555 -t $out/bin pp2n

    # some various dynamic dependencies stuff
    wrapProgram $out/bin/pp2n \
      --prefix PP2N_SRC : $src \
      --prefix PATH : $out/bin:${pkgs.lib.makeBinPath [
        pkgs.coreutils
        pkgs.jq
        pkgs.nix
        pkgs.nix-prefetch-git
      ]}

    # ...
  '';
}
```

And that's it.

## Conclusion

Hopefully this has shown you that writing derivations that simply call GHC are not too hard to write, and that overcoming the typical version bounds mismatch problem is not too bad if you dig into the details some.

Admittedly, the Haskell documentation for NixPkgs does not contain information on how to use these lib functions, and what properties all exist and how they work. I hope this post will still be useful for future readers even if NixPkgs were to change the Haskell infrastructure in the future, as some kind of pole star for what to look for in NixPkgs.

## Links

* Nixpkgs Haskell guide (does not contain information in this blog post) <https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure>
