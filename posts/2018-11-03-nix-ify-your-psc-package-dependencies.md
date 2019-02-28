If you know how Psc-Package, like many other "package managers", works by cloning a bunch of git repositories specified at versions by solving out a dependency list, this seems like a waste of resources and time if things are constantly refetched. While other package managers that work this way sometimes introduce hacky caching schemes, even well known projects do not do so for both the sake of straightforward usage and lack of committed implementers.

Thankfully, we can get rid of this problem from one step up the chain by fetching the git repositories ourselves using Nix, and the caching and storing of these packages is left to Nix to handle. Thus, I made a Psc-Package2Nix project that generates the derivations for packages declared in our `psc-package.json` files.

## Brief introduction to Psc-Package

I've written some information on how Psc-Package works in the Spacchetti documentation here: <https://spacchetti.readthedocs.io/en/latest/intro.html>

In short, there are two things that we care about in Psc-Package:

* The package set we are using, defined by `set` (git tag or branch) and `source` (git repo URL) in the `psc-package.json`
* The list of *direct* dependencies we have, in the `depends` property of `psc-package.json`.

When Psc-Package clones our package set, it goes into `.psc-package/${set-name}/.set`, so the package set, which is the set of all packages we will work with, goes in `.psc-package/${set-name}/.set/packages.json`.

Then using this package set, the entire set of dependencies must be solved by traversing the direct dependencies and their dependencies, where then the total set of dependencies is installed into directories of the structure `.psc-package/${set-name}/${dependencyName}/${dependency.version}` from the following form:

```js
{
  // ...
  "some-library": {
    "dependencies": [
      "functions",
      "parallel",
      // ...
    ],
    "repo": "https://github.com/user/purescript-some-library.git",
    "version": "v1.0.0"
  }
  // ...
}
```

## Derivations for each dependency

To define how each of these dependencies are going to be downloaded and put in the store, we need a Derivation. There are some very lengthy descriptions of how to write derivations and how they work (see Links section below), for our purposes, we only need to know a few things.

* Derivations are descriptions of how to get some *source* input into the *output*
* There exists a generic setup shell script that defines some various phases of a derivation's runtime. Of these, we mostly only need to know about the Build and Install phases.

And so, we can write some templates for how we want to generate a `packages.nix`

```pl
my $file_template = <<END;
{ pkgs ? import <nixpkgs> {} }:

let
  inputs = {
DERIVATIONS};

in {
  inherit inputs;

  set = SET;
  source = SOURCE;
}
END
```

And so, in our template we will expose the set and source information of our package set and the inputs as a large set, where we will insert derivations. This expression also allows for specifying the `pkgs`, but will use `<nixpkgs>` by default.

Then we have each derivation, which will be defined by this template:

```pl
my $derivation_template = <<END;

    DEP = pkgs.stdenv.mkDerivation {
      name = NAME;
      version = VERSION;
      src = pkgs.fetchgit {
        url = REPO;
        rev = REV;
        sha256 = HASH;
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r \$src \$out";
    };
END
```

So here, each derivation will use `fetchgit` to set the src location contents and copy over the contents to the output directory in the Build phase of the derivation. Then the installation phase is disabled by setting `dontInstall`, so that Nix doesn't error on trying to run `make` or anything like that.

And that makes up the basis of what we will generate in `packages.nix` from Psc-Package2Nix. We will look later at how we choose to consume these packages.

## The implementation of Psc-Package2Nix

To implement Psc-Package2Nix, I went with two of my favorite tools: `jq` and Perl. Overall, there are some small stages that this program runs in:

* Ensure that the package set has been checked out into `.psc-package/$set/.set/packages.json`.
* Figure out the total dependency set by querying the information about each direct dependency and their dependencies.
* Fork a bunch of processes to use `nix-prefetch-git` to get the sha256 hash needed to fetch the dependencies.
* Build up the derivations templates using the dependency information with the hashes.
* Write the `packages.nix` file using the template.

### Ensuring the package set

This part is fairly straightforward, where I check for the package set existing and clone it out if not:

```pl
chomp(my $source = `jq '.source' psc-package.json -r`);
chomp(my $set = `jq '.set' psc-package.json -r`);

my $json = ".psc-package/$set/.set/packages.json";

unless (-e $json) {
    print `git clone -c advice.detachedHead=false --branch $set --depth 1 $source .psc-package/$set/.set`;

    if ($? != 0) {
        die "Error on downloading package set";
    }
}
```

### Solving the dependency set

To install the dependencies, we need to visit a bunch of dependencies. We can skip any we have already run into, but we need to at least try to look at all dependencies of dependencies recursively. Finally, we can take the dependencies we have visited to get the list of all dependencies.

```pl
my $config = 'psc-package.json';
my @direct_deps = `jq '.depends | values[]' $config`;
my %visited = ();
sub getDeps {
    my ($name) = @_;
    chomp($name);
    if ($visited{$name}) {
        return;
    }
    $visited{$name} = 1;
    my @transitive_deps = `jq '.${name}.dependencies | values[]' $json`;
    foreach my $target (@transitive_deps) {
        getDeps($target);
    }
}
foreach my $target (@direct_deps) {
    getDeps($target);
}

my @deps = sort keys %visited;
```

### Prefetching dependencies

Prefetching the dependencies is the same as getting the cloned copies, where we check if the hash has already been prefetched and prefetch it if it has not. Here, we will also save some information about dependencies in hashes, since we will need to dig them up later unless we want to refetch them (which would be fine too).

```pl
my $dir = ".psc-package2nix";
my %targets = ();
my %versions = ();
my %repos = ();
my @pids;

# ensure I have my hashes downloaded
foreach my $depQuoted (@deps) {
  (my $dep = $depQuoted) =~ s/"//g;
  chomp(my $version = `jq '.$depQuoted.version' $json -r`);
  chomp(my $repo = `jq '.$depQuoted.repo' $json -r`);
  my $target = "$dir/$dep-$version";

  $versions{$dep} = $version;
  $targets{$dep} = $target;
  $repos{$dep} = $repo;

  unless (-e $target) {
    # fork the process at this point
    my $pid = fork;
    if (not defined $pid) {
        die "Couldn't fork new child processes\n";

    } elsif ($pid == 0) {
        # this branch is for the actual running child process
        say "fetching $target";
        print `nix-prefetch-git $repo --rev $version --quiet | jq '.sha256' -r > $target`;

        if ($? != 0) {
            die "Nix-Prefetch-Git failed at $target";
        } else {
            exit;
        }
    } else {
        # this branch is the parent with the child process pid
        push @pids, $pid;
    }
  }
}

# we wait for all of the pids to complete sequentially here
for my $pid (@pids) {
    waitpid $pid, 0;
}
```

### Building the derivations

We can build up our derivations now by using the hashes that have been fetched. This then takes the derivation template from before to add to our total derivations.

```pl
my $derivations = "";

# build up my derivations
foreach my $depQuoted (@deps) {
  (my $dep = $depQuoted) =~ s/"//g;
  my $version = $versions{$dep};
  my $target = $targets{$dep};
  my $repo = $repos{$dep};

  chomp(my $hash = `cat $target`);

  my $derivation = $derivation_template;
  $derivation =~ s/DEP/$dep/;
  $derivation =~ s/NAME/"$dep"/;
  $derivation =~ s/VERSION/"$version"/;
  $derivation =~ s/REPO/"$repo"/;
  $derivation =~ s/REV/"$version"/;
  $derivation =~ s/HASH/"$hash"/;

  $derivations .= $derivation;
}
```

### Writing the file

Writing our file is then fairly simple, where we put the derivations into our file template.

```pl
my $file = $file_template;
$file =~ s/DERIVATIONS/$derivations/;
$file =~ s/SET/"$set"/;
$file =~ s/SOURCE/"$source"/;

my $filename = 'packages.nix';
open(my $fh, '>', $filename) or die "Could not get file handle for $filename";
print $fh $file;
close $fh;

say "wrote $filename";
```

There we have it, the whole shebang.

## Usage

### Test project setup

In the `test/` directory of the Psc-Package2Nix repository, we can see a usage of Psc-Package2Nix. First, there is a `default.nix` that we can use for nix-shell, which will bring in the things we need from [easy-purescript-nix](https://github.com/justinwoo/easy-purescript-nix), which I wrote about before [here](https://qiita.com/kimagure/items/de2a4ff45dd8fe8be4b1), and a remote import of `psc-package2nix`:

```nix
let
  pkgs = import <nixpkgs> {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "347ab7c91634462c2039c6c0641af5386c251a98";
    sha256 = "0njhcl7dq58b3kmjbz6ndsccv4pcmdxc5lg7p13115phcmznpn99";
  });

  # at the root of the psc-package2nix repo we have a default.nix
  # which defines this derivation and allows for overriding pkgs
  psc-package2nix = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "psc-package2nix";
    rev = "414ba2f58e270dece3834021e380c41cd940b983";
    sha256 = "0lrw2k1gm4aamnlxi16syibyqi7i3nvx9bwzq889vd1p0sbzxs9x";
  }) {};

in pkgs.stdenv.mkDerivation {
  name = "test";
  src = ./.;

  buildInputs
    = [
      pkgs.jq
      pkgs.nix-prefetch-git
      easy-ps.inputs.purs
      easy-ps.inputs.psc-package-simple
      psc-package2nix
    ];

}
```

Then, we have a Makefile for running some tasks:

```
default:
	nix-shell --run 'make build'
build:
	psc-package2nix
	nix-shell install-deps.nix --run 'echo installation complete'
	psc-package build
```

So here, we can see that we will run using `install-deps.nix` separately.

### `install-deps.nix`

In this file, we will define a derivation that pulls in the dependencies generated to `packages.nix`, along with a task to copy over the dependencies to where they need to go for Psc-Package to consume them. Then, we can use a `shellHook` to copy the dependencies over from the store into local files we can use and edit if needed.

```nix
let
  pkgs = import <nixpkgs> {};

  packages = import ./packages.nix {};
  packageDrvs = builtins.attrValues packages.inputs;

  copyCmds = map (x: let target = ".psc-package/${packages.set}/${x.name}/${x.version}";
    in ''
      mkdir -p ${target}
      cp --no-preserve=mode,ownership,timestamp -r ${toString x.outPath}/* ${target}
    '') packageDrvs;

in pkgs.stdenv.mkDerivation {
  name = "install-deps";
  src = ./.;

  buildInputs = packageDrvs;

  shellHook = toString copyCmds;
}
```

And that's it! We can run `make` in this test repo and see everything run.

```
> make
nix-shell --run 'make build'
make[1]: Entering directory '/home/justin/Code/psc-package2nix/test'
psc-package2nix
[ Cloning into '.psc-package/... ]
[ fetching .psc-package2nix/... ]
wrote packages.nix
nix-shell install-deps.nix --run 'echo installation complete'
installation complete
psc-package build
[ Compiling ... ]
make[1]: Leaving directory '/home/justin/Code/psc-package2nix/test'
```

Cool! We can also see how this has run in Travis here: <https://travis-ci.com/justinwoo/psc-package2nix/builds/90154766>

## Conclusion

Hopefully this has shown you how Psc-Package2Nix works, and how you can use the output as it exists today. While some details of how to consume the generated output may change in the future, the fundamental operations and details of this should not change.

And really, with this, once you have generated the `packages.nix` file in any other projects you have, you no longer need to download any more copies of libraries at given versions.

## Links

* This project, Psc-Package2Nix <https://github.com/justinwoo/psc-package2nix>
* Easy-PureScript-Nix: <https://github.com/justinwoo/easy-purescript-nix>
* Nix manual on derivations <https://nixos.org/nix/manual/#ssec-derivation>
* Nix blog post on your "first" derivation <https://nixos.org/nixos/nix-pills/our-first-derivation.html>
* NixPkgs Generic Setup shell script <https://github.com/NixOS/nixpkgs/blob/6742bdc84595525f8aedc11553493fef70621c31/pkgs/stdenv/generic/setup.sh>