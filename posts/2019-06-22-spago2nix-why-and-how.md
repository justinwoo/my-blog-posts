# Spago2Nix: Why and How

Recently, I spent my time at ZuriHac making Spago2Nix, a tool to generate a Nix expression of PureScript dependencies specified by Spago. In this post, I'll talk about what I wanted to get done with this tool, and how I went about it.

## Generating a Nix expression of packages

First off, I need to get an expression built of dependencies needed to build our project. Spago makes this easy enough with a bunch of flag soup:

```
spago list-packages -f transitive -j # -f: filter by, -j: in JSON format
```

And of course, I need to prefetch the git repos with the revision information along with the contents hash I need for Nix, so I do this using `nix-prefetch-git`:

```
nix-prefetch-git {url} --rev {version} --quiet # --quiet will give me JSON
```

Of course, using the --quiet flag with nix-prefetch-git will happily fetch you an empty repo, so you must be sure to check if you have cloned an empty repo:

```purs
brokenRepoSHA :: SHA256
brokenRepoSHA = SHA256 "0sjjj9z1dhilhpc8pq4154czrb79z9cm044jvn75kxcjv6v5l2m5"
```

Once I have this information, then each package can be built up:

```nix
    "PKGNAME" = pkgs.stdenv.mkDerivation { # PKGNAME e.g. aff
        name = "PKGNAME";
        version = "VERSION"; # VERSION e.g. v1.0.0
        src = pkgs.fetchgit {
          url = "URL"; # URL e.g. https://github.com/someone/aff.git
          rev = "REV"; # REV being some SHA
          sha256 = "SHA256"; # SHA256 being the prefetch SHA contents
        };
        phases = "installPhase";
        installPhase = "ln -s $src $out";
      };
```

The reason I use this method of defining derivations is because I want to be able to represent the same content as the repo fetch, but be free to change the metadata around the cloned repo as needed. I have found overriding the properties to create new entries to be wasteful, so this is what I have now.

## Installing these packages

So let's actually think about what "installing" means, because there are multiple different phases involved.

1. A package should be downloaded by Nix into the Nix Store
2. We want to copy this Nix Store entry into our project under `.spago` so Spago can use it, and so we can also modify the sources we use in our project whenever we wish (e.g. for debugging).

So the first step can be performed by building a derivation which uses the information. Which leads us to the second: how should we copy Nix Store entries to our local project?

After much consideration, I found the most realistic answer to be to write a derivation which creates a bash script.

## `installSpagoStyle`

Imagine we started our output template in such a way:

```nix
{ pkgs ? import <nixpkgs> {} }:

let
  inputs = {
INPUTS # these are the packages from above i will shove in
  };
```

With this structure, inputs is a set we could get the attribute values for to map a function for what individual operations we need. So we could very well start writing a bash script derivation like so:

```nix
  installSpagoStyle = pkgs.runCommand "install-spago-style" {} ''
      >>$out echo "#!/usr/bin/env bash"
      >>$out echo
      >>$out echo "echo installing dependencies..."
      >>$out echo "${builtins.toString (
        builtins.map cpPackage (builtins.attrValues inputs))}"
      >>$out echo "echo done."
      chmod +x $out
  '';
```

Another thing we should keep in mind is that the PureScript compiler (as of 0.13.0) is not smart enough about rebuilding: because it uses a timestamp to figure out if corresponding outputs need to be rebuilt. This a defect that will be fixed eventually. Either way:

```nix
  cpPackage = pkg:
    let
      target = ".spago/${pkg.name}/${pkg.version}";
    in ''
      if [ ! -e ${target} ]; then
        echo "Installing ${target}."
        mkdir -p ${target}
        cp --no-preserve=mode,ownership,timestamp -r ${toString pkg.outPath}/* ${target}
      else
        echo "${target} already exists. Skipping."
      fi
    '';
```

Of course, I have to use the no-preserve flags because copying straight from the Nix Store will give you unusable files by default.

## Building the project

Same thing but with purs compile, and taking in any more args accordingly:

```nix
  getGlob = pkg: ''\".spago/${pkg.name}/${pkg.version}/src/**/*.purs\"'';

# ...

  buildSpagoStyle = pkgs.runCommand "build-spago-style" {} ''
      >>$out echo "#!/usr/bin/env bash"
      >>$out echo
      >>$out echo "echo building project..."
      >>$out echo "purs compile \"\$@\" ${builtins.toString (
        builtins.map getGlob (builtins.attrValues inputs))}"
      >>$out echo "echo done."
      chmod +x $out
  '';
```

## Implementation

All of this is implemented as a simple PureScript-Node program that calls out to the various programs needed:

```purs
foreign import argv :: Array String

args :: List String
args = List.drop 2 $ List.fromFoldable argv

main :: Effect Unit
main = Aff.launchAff_ do
  case args of
    "generate" : List.Nil -> Generate.generate
    "install" : rest -> install rest
    "build" : rest -> build rest
    "help" : rest -> log help
    List.Nil -> log help
    _ -> do
      log $ "Unknown arguments: " <> List.intercalate " " args
```

I prebuild this program and have it packaged on npm, but it is also available to install via a fetchFromGitHub derivation:

```nix
in pkgs.stdenv.mkDerivation {
  name = "spago2nix";

  src = ./.;

  buildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    target=$out/bin/spago2nix

    >>$target echo '#!/usr/bin/env node'
    >>$target echo "require('$src/bin/output.js')";

    chmod +x $target

    wrapProgram $target \
      --prefix PATH : ${pkgs.lib.makeBinPath [
        pkgs.nix-prefetch-git
        easy-purescript-nix.purs
        easy-purescript-nix.spago
        easy-dhall-nix.dhall-json-simple
      ]}
  '';
}
```

## Conclusion

I hope this has shown you how Spago2Nix works, and how you can easily extend this to your own usages or make your own tool for your own needs.

## Disclaimer

Honestly though, this is written for my own usage. If you want it to do some other things, you might open some issue on the repo, but I mostly want other people to make PRs to implement features if they really need them. In general, Nix is something that does take some effort to use, so I expect that people who want to use this project are willing to ask some questions and help out. At the very least, help me help you.

## Links

* <https://github.com/justinwoo/spago2nix>
