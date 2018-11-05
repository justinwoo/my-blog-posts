# Make your own Psc-Package with Perl

...and you can even distribute it with Nix!

If you use PureScript, chances are that you've tried using Psc-Package, and you might even have tried my [Spacchetti](https://github.com/justinwoo/spacchetti) package set solution that uses Dhall. But one thing seems missing here... we could be using our Dhall `packages.dhall** directly in our project configuration, if the project configuration were also in Dhall. Since Psc-Package is a tool that allows for anyone to generate things for it to consume, Spacchetti works, but what if we were to just replace Psc-Package? That we can!

*Before we start, I should say that I'm not necessarily recommending that everyone should use Perl scripts for everything, just that it is an option. Some of us who like using Spacchetti are thinking of ways to make a well-made Haskell program that consumes Dhall using the Dhall-Haskell library. But for various reasons and to maximize my own amusement, I chose to do this in Perl. Your own usages might use Python, Ruby, Racket, Haskell, PureScript on Node, or just straight up bash. Sky's the limit!*

## Some background and `packages.dhall`

Previously, I have written about the Spacchetti project and how local package management works in this post: <https://qiita.com/kimagure/items/c419ba740ac134a837a2#local-package-sets>

In short, you can write a `packages.dhall` file like this:

```hs
    let mkPackage =
          https://raw.githubusercontent.com/justinwoo/spacchetti/190618/src/mkPackage.dhall

in  let overrides =
          { typelevel-prelude =
              mkPackage
              [ "proxy", "prelude", "type-equality" ]
              "https://github.com/justinwoo/purescript-typelevel-prelude.git"
              "prim-boolean"
          }

in    https://raw.githubusercontent.com/justinwoo/spacchetti/190618/src/packages.dhall
    ⫽ overrides
```

And Dhall will fetch the remote sources in compilation. So with this, you can use the `packages.dhall` file from Spacchetti if you just have a reference to it like this.

## `spacchetti.dhall`

So what all do we need to set up for a project anyway? For working with a project, there are really just two things we need in a Psc-Package-like that uses a package set definition from Dhall code:

* The packages definition (from above)
* A dependencies list

For the hell of it, we can also add in a "name" property:

```hs
{ name =
    "my-project"
, dependencies =
    [ "effect", "console" ]
, packages =
    ./packages.dhall
}
```

And this is all we need to work with, since Dhall can handle fetching the remote package set for us. Of course, since there is no Dhall-Perl implementation, we will be using `dhall-to-json` to convert into its JSON form when we need to work with this later.

## What features do we need?

To have a Psc-Package clone that is fairly useful, there are about ~4 features we need:

* `init` -- initialize a new project
* `install` -- install our dependencies (and transitive dependencies)
* `sources` -- to get source globs of where our dependencies are, for PureScript compiler (`purs compile`), IDE server (`purs ide`), and others
* `build` -- to build our project without too much trouble without having to fumble with the `sources` command directly.

## Initializing our project

For our project to be set up automatically, we will need some template files: <https://github.com/justinwoo/spaghetto/tree/master/templates>. For my purposes, I will be using [FindBin](http://perldoc.perl.org/FindBin.html) so that I can always access this directory relative to my running scripts. And so, we can write our `spacchetti-init` script in this kind of way:

```pl
#!/usr/bin/env perl

# strict and warnings are very useful :)
use strict;
use warnings;
use FindBin '$RealBin';

print `cp $RealBin/templates/gitignore .gitignore`;
# ...
```

And now we have properly gotten started!

## Installing our project

To install our project, we first need to do two things:

1. Prepare `spacchetti.dhall` so we can easily work with it.
2. Figure out the total set of dependencies we need to install.

### `spacchetti.json`

With Dhall-to-JSON, we can accomplish this quite readily:

```pl
print `cat spacchetti.dhall | dhall-to-json --pretty > $json_path`;
```

Of course, there are some considerations on when this JSON should be generated. In general, we should be willing to override the previous generation especially when we are running `install`, but we should also have some logic for keeping the previous file. We'll call this `spacchetti-make-json`:

```pl
use feature 'say';

print `mkdir -p .spacchetti`;
my $json_path = '.spacchetti/spacchetti.json';

my $keep_existing = $#ARGV > -1 ? $ARGV[0] eq "keep" : 0;
my $exists = -e $json_path;

if ($exists && $keep_existing) {
    exit;
} else {
    print `cat spacchetti.dhall | dhall-to-json --pretty > $json_path`;
    if ($? == 0) {
        say "wrote $json_path";
    }
    # ...
```

This takes care of if we should keep the previous generation by looking for an argument "keep" to our script.

### Total set of dependencies

To get our set of dependencies, we can define a routine to go through every dependency's dependencies, skipping any dependency that we have already seen. We can do this with a hashmap using the keys as our set and print out the keys of our visited dependencies.

To read the `spacchetti.json` file we generated earlier, we can just make use of [jq](https://github.com/stedolan/jq).

With this, we can write `spacchetti-get-deps`:

```pl
# Call make-json with "keep", so we can ensure we have the spacchetti.json available
print `$RealBin/spacchetti-make-json keep`;

my $json_path = '.spacchetti/spacchetti.json';

# these are the direct dependencies we have in our project
my @direct_deps = `jq '.dependencies | values[]' $json_path`;

# an empty hashmap of visited dependencies
my %visited = ();

# our subroutine to get dependencies
sub getDeps {
    my ($name) = @_;
    chomp($name);

    # return early if we have already visited
    if ($visited{$name}) {
        return;
    }

    # otherwise, mark that we have visited it
    $visited{$name} = 1;

    # get the dependencies of this dependency
    my @transitive_deps = `jq '.packages.${name}.dependencies | values[]' $json_path`;

    foreach my $target (@transitive_deps) {
        getDeps($target);
    }
}

foreach my $target (@direct_deps) {
    getDeps($target);
}

# return the dependencies
say for keys %visited;
```

Nice! This will also be useful for the `sources` command later.

### Installing a dependency

Now that we can get our total dependencies, let's first install an individual package.

We know that a given "package" entry will have a "repo" property, which is the repository URL, and a "version" property, which is a tag or branch ref to retrieve the dependency by. Then, we will want to check out the contents to a directory in the form `.spacchetti/${name}/${version}`.

We can call this `spacchetti-install-package`:

```pl
my $name = $ARGV[0];
chomp($name);

my $json_path = '.spacchetti/spacchetti.json';

my $version = `jq '.packages."$name".version' $json_path -r`;
chomp($version);

my $repo = `jq '.packages."$name".repo' $json_path -r`;
chomp($repo);

my $target = ".spacchetti/$name/$version";

unless (-e $target) {
    print `mkdir -p $target`;

    # git soup
    print `cd $target && git init && git remote add origin $repo && git fetch --depth 1 origin $version && git -c advice.detachedHead=false checkout FETCH_HEAD`;

    if ($? == 0) {
        say "Installed $target";
    } else {
```

Btw, did I mention that trying to fetch a specific SHA from git is basically impossible? You can try to look into various reasons for this, but mainly...

1. the option to allow clients to ask for specific SHA was introduced in 2015
2. GitHub does not allow you to use this, and makes you to use their API (otherwise, it "seem" to work for some arbitrarily new commits, but not older ones)
3. the workaround is to clone a repository and try to find your specific SHA.

But let's not be too surprised about Git not working as well as we'd like.

### Installing all of our dependencies

Now we can start installing our packages. While we could sequentially install our packages, I think it would be more fun to make a farm of forked processes to run the installation operations. There's not too much involved here other than to call the `install-package` script on each dependency of our total set of dependencies in `spacchetti-install`:

```pl
my @deps = `$RealBin/spacchetti-get-deps`;

my $num = $#deps + 1;

say "Installing $num packages";

my @pids;

foreach my $target (@deps) {
    chomp($target);

    my $pid = fork;

    if (not defined $pid) {
        die "Couldn't fork new child processes\n";
    } elsif ($pid == 0) {
        print `$RealBin/spacchetti-install-package $target`;

        if ($? != 0) {
            die "Installation failed at $target";
        } else {
            exit;
        }
    } else {
        push @pids, $pid;
    }
}

for my $pid (@pids) {
    waitpid $pid, 0;
}

say "Installed $num packages."
```

Amazing! This will install our packages at lightning speed.

## Source globs for our dependencies

Now that the hard part is finished, we can generate the source globs for our dependencies. Since we have downloaded our dependencies to `.spacchetti/${name}/${version}`, we can just generate globs according that pattern for `spacchetti-sources`:

```pl
my @deps = `$RealBin/spacchetti-get-deps`;

my @globs = ();

sub makeGlob {
    my ($name) = @_;
    chomp($name);
    $name =~ s/"//g;

    my $version = `jq '.packages."$name".version' $json_path -r`;
    chomp($version);

    my $glob = "\".spacchetti/${name}/${version}/src/**/*.purs\"";

    push @globs, $glob;
}

foreach my $target (@deps) {
    makeGlob($target);
}

say for @globs;
```

## Building our project

Now that we have the globs, we can pass these to `purs compile`. We should remember to also pass in the source and test directory globs, but after that, we're done.

```pl
my @globs = `$RealBin/spacchetti-sources`;

my $args = '"src/**/*.purs" "test/**/*.purs"';

foreach my $glob (@globs) {
    chomp($glob);
    $args .= " $glob";
}

print `purs compile $args`;

if ($? == 0) {
    say "Build succeeded.";
} else {
    die "Build failed.";
}
```

And there we have it, we have finished up the stages of writing up our own Psc-Package-like.

## Consuming our Perl scripts

Now consuming this seems like it'd end up being a lot of work. We have to somehow make sure that the scripts will be in our PATH and that the `templates/` directory will be somewhere relative to those, but that seems really error prone since we don't know for sure where it's going to end up or anything. Thankfully, we can make this easy for ourselves: by using Nix.

Previously, I wrote about how to use PureScript and related tools here: <https://qiita.com/kimagure/items/de2a4ff45dd8fe8be4b1>. In this post, I talked some about how to make derivations and consume them, which we'll do here.

For one, we know that the derivation source we want will be the repository where we push our Perl scripts up to (or a relative path, pick your poison). Then we can use `nix-fetch-git`/`nix-prefetch-github` to get the latest revision and its hash. Then for our install phase, we know that we just need to copy some files over to our output directory under `bin/`, and the binaries placed in the output will be made available to us, so our derivation ends up being fairly short:

```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  name = "spaghetto";

  src = pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "spaghetto";
    rev = "3b8fd2beeb4177afbc7f65e7a547c35bcb426827";
    sha256 = "1qnwnfci81hlv7m39hn0jf6znpjkfzl8s7ppr9ibabfazfk631pj";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp spacchetti-* $out/bin
    cp -r templates $out/bin/templates
  '';
}
```

We can save this in a `spaghetto.nix` file and then go ahead with writing a `default.nix` with the tools from easy-purescript-nix that we need:

```nix
let
  pkgs = import <nixpkgs> {};

  spaghetto = import ./spaghetto.nix {};

  easy-ps = import (pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "easy-purescript-nix";
    rev = "45050a69fc60b9e7259053a9d68d42a0e47dbf33";
    sha256 = "1ajnsiclzv4xcjd6dn6p8bwqmyjib7rjrya0m57gj1dwkzn9z3lk";
  });

in pkgs.stdenv.mkDerivation {
  name = "spag-test";
  src = ./.;

  buildInputs = [spaghetto] ++ easy-ps.buildInputs;
}
```

And now that we have this set up with the build inputs all set up, all we have to do is write a Makefile that contains some things we want:

```Makefile
default: build run

build:
	spacchetti-init
	spacchetti-install
	spacchetti-sources
	spacchetti-build

run:
	node -e "require('./output/Main').main()"

nix:
	nix-shell --run "make"
```

And voila! We can try running `make nix` on this and everything will run as expected:

```
> make nix
nix-shell --run "make"
# nix stuff
make[1]: Entering directory '/home/justin/Code/spag-test'
spacchetti-init
initialized Spacchetti project.
spacchetti-install
wrote .spacchetti/spacchetti.json
Installing 3 packages
Installed 3 packages.
spacchetti-sources
".spacchetti/prelude/v4.1.0/src/**/*.purs"
".spacchetti/effect/v2.0.0/src/**/*.purs"
".spacchetti/console/v4.1.0/src/**/*.purs"
spacchetti-build
Compiling # ...
Build succeeded.
node -e "require('./output/Main').main()"
Hello Spacchetti!
make[1]: Leaving directory '/home/justin/Code/spag-test'
```

Cool!

## Conclusion

I hope this has shown you how Psc-Package works in general, and hopefully has given you some ideas on what you might try doing yourself sometime, whether it's making your solution, contributing to Psc-Package bugfixes, contributing to my Spacchetti-CLI project, or something beyond.

If anything, I hope this has shown you that you can make your dreams come true with Perl and Nix.

## Links

* This repo of Perl: <https://github.com/justinwoo/spaghetto>
* Test repo using Nix: <https://github.com/justinwoo/spag-test>

