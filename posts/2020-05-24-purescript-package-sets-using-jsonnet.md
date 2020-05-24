# PureScript package sets using Jsonnet

Back in 2018, I decided to tackle some problems some people had with package sets and ended up using Dhall to try to solve these problems in ["Managing Psc-Package sets with Dhall"](https://github.com/justinwoo/my-blog-posts/blob/9316a7e2049e20b77084e766961d61f9f8e30600/posts/2018-06-22-managing-psc-package-sets-with-dhall.md).

However, in the last two years I have found that I do not enjoy using this project and prefer stability, ease of use, and some various other factors. To that end, I ended up using a fairly old but still actively maintained language to try to make this easier: [Jsonnet](https://jsonnet.org/). I'm sure all of the Dhall fans have stopped reading this post already with that last sentence, so I will now just go into how using package-sets works with this.

## Package sets repo structure

For this new package sets structure, I have a repo now in https://github.com/justinwoo/package-sets-jsonnet with the following main structure:

```
packages.json -- this is what psc-package can use directly

src/
  packages.jsonnet -- this consumes subsets from groups/ to form the package set

  groups/
    justinwoo.libsonnet -- this defines a subset of packages for the package-set
                        -- in reality, "groups" are a fake organizational structure
                        -- that roughly corresponds to "github user/org"
```

### "Groups" libsonnet

These Jsonnet "lib" files contain individual packages as usual:

```jsonnet
{
  chirashi: {
    dependencies: [
      'exceptions',
      'prelude',
      'typelevel-prelude',
      'variant',
    ],
    repo: 'https://github.com/justinwoo/purescript-chirashi.git',
    version: 'v1.0.0',
  },
  // ...
```

### `packages.jsonnet`

This file then just combines these packages as usual.

```jsonnet
local justinwoo = import 'groups/justinwoo.libsonnet';
local purescript = import 'groups/purescript.libsonnet';
// more imports...

local packages =
  purescript
  + justinwoo
  // more combinations ...
  ;

packages
```

## Usage

And so, like with package-sets, you could consume upstream package sets from this repo or choose to fork the upstream and rebase changes onto it. Rebasing changes ends up being rather easy, as you need to only merge a few entries in any case. But if managing another git repo sounds too annoying to you or you need to have specific overrides for your project, you might consider some various methods of doing so.

Some people will opt for vendoring or submodules, which are fine. I will demonstrate another possible usage using Nix.

## jsonnet-packages-consumer

I've created an example repo here: https://github.com/justinwoo/jsonnet-packages-consumer

This project will quite simply use Nix to both fetch the upstream package sets and build the result package set from a derivation. The project structure is as follows:

```
shell.nix -- just because i do not have jsonnet on my path
default.nix -- derivation for building our package set JSON output
overrides.jsonnet -- a jsonnet source file that will build our package set
```

You can guess the `shell.nix` will have nothing in it:

```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [ pkgs.jsonnet ];
}
```

The derivation itself gets more interesting, where it fetches package-sets-jsonnet to provide to jsonnet as a library path:

```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.runCommand "jsonnet-packages-consumer" {
  src = pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "package-sets-jsonnet";
    rev = "b347f41013f7fa5598a2e6c0ef5f4613b5277195";
    sha256 = "1l7xx265bkrg5133mja3y81bpdwimsjibmd017bipqyp46fsd93f";
  };

  buildInputs = [ pkgs.jsonnet ];
} ''
  mkdir -p $out
  jsonnet -J $src/src ${./overrides.jsonnet} -o $out/packages.json
''
```

As you can see, the `-J` flag will pass in more lib search paths, which will be used from our `overrides.jsonnet` source:

```jsonnet
local upstream = import 'packages.jsonnet';

upstream {
  'simple-json': upstream['simple-json'] { version: 'override' },
}
```

Remember that the packages-set Jsonnet file with the package set definition was in `packages.jsonnet`, and this is what is being imported here by providing it in the jsonnet library search path. Then, we are using the override syntax in order to refine `simple-json` and change the `version` attribute. We can see the result when we then build this derivation:

```
$ ls result
packages.json

$ jq 'length' result/packages.json
369

$ jq '."simple-json"' result/packages.json
{
  "dependencies": [
    "arrays",
    "exceptions",
    "foreign",
    "foreign-object",
    "globals",
    "nullable",
    "prelude",
    "record",
    "typelevel-prelude",
    "variant"
  ],
  "repo": "https://github.com/justinwoo/purescript-simple-json.git",
  "version": "override"
}
```

Fairly straightforward. This derivation could be built as part of some project's setup or the built result could be vendored into a repo for use directly from the project sources.

## Conclusion

Hopefully this has shown you another way to work with package sets that doesn't have to involve using Dhall or whatever other tools. Consumers of package sets defined in this way can continue to use Psc-Package or any other setup making use of the package set JSON without any changes, while the generation of the package set JSON can use any setup you see fit.

Any activity on this repo mostly depends on my own usage and the help of others. If you like this idea and would like to see it gain more traction, please say something in an appropriate way and come help. If you don't like this project, then you are free to not use it, as it may not at all impact how you use anything you currently do.

Even though I am to blame for the current situation in which package sets and some other tooling has ended up consuming Dhall as a result of package sets' own usage, I personally do not plan to use Dhall in the future in this case or many other cases. I can only ask that you accept that I came to this conclusion and refrain from sending me hate mail, further inquiries on why I do not like Dhall, or otherwise then trying to compare me to other kinds of evil you see in the world like anti-vaxxers, anti-lockdown protesters, and the like which seems so common when in any kind of conversations with people discussing technology online.

## Links

* package-sets-jsonnet: https://github.com/justinwoo/package-sets-jsonnet
* Jsonnet: https://jsonnet.org/
* jsonnet-packages-consumer: https://github.com/justinwoo/jsonnet-packages-consumer
