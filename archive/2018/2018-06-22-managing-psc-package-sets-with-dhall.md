# Managing Psc-Package sets with Dhall

Some people who use PureScript may use Psc-Package to manage their dependencies, which works by having a package set, which is a collection of packages defined at specific versions, which can then depend on each other simply by name.

At ZuriHac, I found myself in the Dhall room for a while, and thought that it might be interesting to try using it to generate my package set, and I had a working version within a couple of hours starting from near-zero knowledge of what Dhall could do. I've published the results in [Spacchetti](https://github.com/justinwoo/spacchetti), and I've been using it already to generate package sets.

## The "problem" with PureScript/Package-Sets

When working with Psc-Package normally using purescript/package-sets, you'll start to notice a couple of things:

* Psc-Package is basically a glorified set of shell scripts
* Only having a packages.json file in the repo you point to from psc-package.json matters
* packages.json is pretty terrible to edit since it's a giant blob, meaning that every time you rebase your package-sets fork to "update", you get a giant useless diff\
* packages.json isn't checked in any way, it's just your typical dynamically typed hell
* purescript/package-sets isn't even "official" in any meaningful way -- it's just some community-gathered information
* you should *absolutely* have your own fork of package-sets to manage package sets for your projects, which is unlike spoon-fed solutions out there

With all of these negative characteristics, it only makes sense to not use purescript/package-sets, right? We might as well look for a solution that is...

* Can merge multiple sets of packages easily
* Typed
* Easy to use
* Can maybe even support a local overrides workflow, for when you need specific overrides that won't be shared with other projects you have

Luckily, those goals are fairly easy to target with Dhall.

## How

There are three main files in my setup on Spacchetti:

* Package.dhall -- this defines the type, which is just a simple type for what fields the packages should have
* mkPackage.dhall -- this is a simple function for constructing `Package`s, which isn't really required
* packages.dhall -- this is a file that will import multiple package sets and merge them

You will also need `dhall` and `dhall-json` installed, which I think you'll find better documentation on the dhall-lang repo: <https://github.com/dhall-lang/dhall-lang>

### Package.dhall

As expected, this file only has the type definition inside:

```hs
{ dependencies : List Text, repo : Text, version : Text }
```

So every `Package` should have a list of names of packages as dependencies, the repo that we'll refer to which is usually a URL, and a version which is the tag or branch that we'll refer to. These correspond directly to what we want in the resulting packages.json.

### mkPackage.dhall

This is just a helper function for creating `Package`s, so the definition is largely repetitive:

```hs
  λ(dependencies : List Text)
→ λ(repo : Text)
→ λ(version : Text)
→ (   { dependencies = dependencies, repo = repo, version = version }
    : ./Package.dhall
  )
```

### packages.dhall

This is where the bulk of the work gets done, where we combine a bunch of package definition records, preferring the right side:

```hs
  ./groups/purescript.dhall
⫽ ./groups/purescript-contrib.dhall
⫽ ./groups/purescript-web.dhall
⫽ ./groups/purescript-node.dhall
⫽ ./groups/slamdata.dhall
⫽ ./groups/lumihq.dhall
⫽ ./groups/community.dhall
⫽ ./groups/natefaubion.dhall
⫽ ./groups/paf31.dhall
⫽ ./groups/justinwoo.dhall
⫽ ./groups/patches.dhall
```

So in this `groups` directory, each file contains packages for a given "group", which is my way of separating the packages. My local patches come last here, because I intend for them to override existing definitions in the other files.

Then each record of packages is defined by using the `mkPackage` function defined earlier:

```hs
    let mkPackage = ./../mkPackage.dhall

in  { arrays =
        mkPackage
        [ "bifunctors"
        , "control"
        , "foldable-traversable"
        , "maybe"
        , "nonempty"
        , "partial"
        , "prelude"
        , "st"
        , "tailrec"
        , "tuples"
        , "unfoldable"
        , "unsafe-coerce"
        ]
        "https://github.com/purescript/purescript-arrays.git"
        "v5.0.0"
    , assert =
        mkPackage
        [ "console", "effect", "prelude" ]
        "https://github.com/purescript/purescript-assert.git"
        "v4.0.0"
        -- ...
```

## Packages.json generation

I then have a generate.sh file for running a `dhall-to-json` command:

```sh
dhall-to-json --pretty <<< "./src/packages.dhall" > packages.json
echo generated to packages.json
```

And then after I check in this packages.json file into my repository, I'm done!

## Usage

The usage of these package sets is now quite the same as usual:

```json
{
  "name": "purescript-web-audio-player-demo",
  "set": "160618",
  "source": "https://github.com/justinwoo/spacchetti.git",
  "depends": [
    "console",
    "halogen",
    "prelude",
    "web-dom"
  ]
}
```

So not much has changed here and it works as expected.

## "Local package sets"

In psc-package, there is nothing like "extra-deps" from Stack. Even though it's editing a package set isn't hard, it can be fairly meaningless to have a package set that differs from package sets that you use for your other projects. While there's no real convenient way to work with it with standard purescript/package-sets, this is made easy with Dhall again where you can define a packages.dhall file in your repo and refer to remote sources for `mkPackage` and some existing packages.dhall.

For example:

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

With this, I can then run a small script to set up my package set:

```sh
NAME='local'
TARGET=.psc-package/$NAME/.set/packages.json
mkdir -p .psc-package/$NAME/.set
dhall-to-json --pretty <<< './packages.dhall' > $TARGET
echo wrote packages.json to $TARGET
```

Then I'm ready to consume this by making a psc-package.json file as usual, but with no source url set:

```json

{
  "name": "my-project",
  "set": "local",
  "source": "",
  "depends": [
    "console",
    "effect",
    "prelude",
    "typelevel-prelude"
  ]
}
```

And in this example, the typelevel-prelude downloaded will be the one from my overrides.

## Conclusion

Hopefully this has shown you how you can work with Dhall to easily generate package sets and be able to work with them in various ways. If nothing else, hopefully this has shown how Psc-Package expects to work with package sets, and how purescript/package-sets is just another source of packages for psc-package, and that you can simply write your own package set.

In the future, it might also be interesting to use my Spacchetti repo or something else to generate nix scripts for working with dependencies through [dhall-nix](https://github.com/dhall-lang/dhall-nix) or something to not have to use psc-package at all.

## Links

* Spacchetti <https://github.com/justinwoo/spacchetti>
* Dhall <https://github.com/dhall-lang/dhall-lang>
