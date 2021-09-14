# PureScript on Nix without dependency codegen

Recently, I wrote about how to work with PureScript on Nix without any package mangers here: <https://github.com/justinwoo/my-blog-posts/blob/master/posts/2020-05-31-working-with-purescript-package-sets-with-just-nix.md>. But this approach still has one big wart: it still relies on codegen to pin packages that are going to fetched for the build, even though the transitive dependency graph can be solved easily and quickly in Nix. What if we could work with PureScript on Nix without having to deal with ugly codegen phases, and instead only have relevant Nix code in our repository?

## Why is it that we need to prefetch packages anyway?

When we think of our dependencies as a collection of individual packages, we are going to need to make sure that Nix can supply them to us when we build our derivations. In order to do so and verify the files we are fetching, we end up with using impure tools to perform codegen so that we can fetch the dozens or hundreds of dependencies that we will need.

But the way we work with other dependencies is quite different, as clearly seen when we are using packages from easy-purescript-nix or nixpkgs. There, we only provide a single pin point for where packages can be fetched from, and we don't necessarily need to care if those are further extra derivations or files that we work with.

What if we could apply the same idea to how we work with PureScript packages?

## Package set archive

So then I decided that this can be solved basically by having a project that manages a package set, provides the prefetched nix derivations for all of the packages, and also vendors them to reduce work required to actually work with the dependencies, all in one.

### Package set definition

I wrote previously about using Jsonnet to define package sets here: <https://github.com/justinwoo/my-blog-posts/blob/master/posts/2020-05-24-purescript-package-sets-using-jsonnet.md>.

Using this method, I created a Nix Shell script to generate packages.json with some modifications:

```nix
#!/usr/bin/env nix-shell
#!nix-shell ./packages-json.nix --run exit

{ pkgs ? import <nixpkgs> {} }:
let
  src = pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "package-sets-jsonnet";
    rev = "07a1109d977b173fdf3764cbd274c57be5b562da";
    sha256 = "0rynm6nivhdfwrg0z2iqx3lz9fpwxfjpkaw5va2c8qiya9rb6pfx";
  };
in
pkgs.mkShell {
  buildInputs = [
    pkgs.jsonnet
  ];
  shellHook = ''
    jsonnet -J ${src}/src ${../packages.jsonnet} -o packages.json
  '';
}
```

### Prefetching packages

Then we need to prefetch packages. For this, I used psc-package-nix, using the generate packages script I defined in that project as described here: <https://github.com/justinwoo/my-blog-posts/blob/master/posts/2020-05-31-working-with-purescript-package-sets-with-just-nix.md#generating-nix-expressions>

```nix
#!/usr/bin/env nix-shell
#!nix-shell ./generate-purs-packages.nix --run exit -j 100

{ pkgs ? import <nixpkgs> {} }:
let
  psc-package-nix = pkgs.fetchFromGitHub {
    owner = "justinwoo";
    repo = "psc-package-nix";
    rev = "dcded6dc74c31208995cfb6666da026b1469b660";
    sha256 = "1zyn0d0373l2lprb1am3p1404yznlxrjcl4nwf23mlnyq3xqkzvq";
  };

  json = ../packages.json;
in
pkgs.mkShell {
  buildInputs = [
    pkgs.nix-prefetch-git
    pkgs.nodejs
  ];
  shellHook = ''
    node ${psc-package-nix}/src/generate-purs-packages-nix.js ${json} > purs-packages.nix
  '';
}
```

At this point, this prefetched set could be consumed downstream and used directly.

### Vendoring dependencies

Finally, with all the packages information acquired, all that remains is to take the derivation outputs and copy them into this project.

```nix
#!/usr/bin/env nix-shell
#!nix-shell ./copy-packages.nix --run exit

{ pkgs ? import <nixpkgs> {} }:
let
  purs-packages = import ../purs-packages.nix { inherit pkgs; };
  pkgs-list = builtins.attrValues purs-packages;
  copy-package = x:
    let
      target = "pkgs/${x.name}/${x.version}";
    in "mkdir -p ${target} && cp --no-preserve=mode -r ${x.src}/* ${target};\n";
  copy-packages = builtins.map copy-package pkgs-list;
in
pkgs.mkShell {
  shellHook = ''
    ${builtins.toString copy-packages}
  '';
}
```

Now we don't have to depend on fetching individual repos to get dependencies.

## Usage

Then our usage can pull in all the PureScript dependencies we need in one go:

```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.fetchzip {
  url = "https://github.com/justinwoo/package-set-archive/archive/849c296b5a682c17977ca1e6165c7dd17af82979.zip";
  sha256 = "08mrycpq75lg5nvvkgz0yj2ll8rbgdk63afmpxg04gma7wqqklzz";
}
```

And this repo contains not only the packages we want to work with, but also the original JSON package set, so we can work with this information in psc-package-nix.

The rest of our usage is the same as with psc-package-nix before, but with two simple changes: 1) we should bring in packages.json from package-set-archive, 2) we can directly define what inputs are required in Nix.

```nix
{ pkgs ? import <nixpkgs> { } }:
let
  easy-ps = # ...
  psc-package-nix = # ...
  package-set-archive = import ./package-set-archive.nix { inherit pkgs; };

  inputNames = [
    "aff"
    "bonjiri"
    "calpis"
    "choco-pie"
    "console"
    "debug"
    "effect"
    "makkori"
    "naporitan"
    "react-basic"
    "simple-json"
    "simple-json-utils"
    "string-parsers"
    "test-unit"
  ];

  packagesJson = "${package-set-archive}/packages.json";

  solved = import "${psc-package-nix}/nix/solve-set.nix" {
    inherit pkgs packagesJson inputNames;
  };

in solved
```

And there we have it. We can then use that solved set in whatever way we please.

Currently, I have some PRs that need to be merged in order to make working in this way more pleasant, but hopefully they will be merged soon. See https://github.com/nwolverson/purescript-language-server/pull/75 and https://github.com/purescript-emacs/psc-ide-emacs/pull/192.

## Conclusion

Hopefully this has given you some ideas on how you might want to work with your own PureScript projects using Nix and without dependency codegen in your project.

## Links

* Psc-Package-Nix: <https://github.com/justinwoo/psc-package-nix>
* Package set archive: <https://github.com/justinwoo/package-set-archive>
