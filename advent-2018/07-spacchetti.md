For a long while now, people have complained that managing a package set file in a giant JSON file is not very nice. While I think editing JSON is at least nice in that it's universal, I agree that overall it isn't that nice.

The other major complaint that people have had is that they don't want to fork the package sets. I think this is kind of silly, but I will at least say the next step of this is rather annoying -- maintaining separate tags if you want to have specific packages that are different between projects, if you want to not have your projects use a common master package set. While you really should consolidate your package sets if you want to have interoperability of modules and encodings between your projects, I admit that there must be *some* kind of better solution out there.

## "Managing Psc-Package sets with Dhall"

In this post, I introduced my Spacchetti project: <https://github.com/spacchetti/spacchetti>

<https://qiita.com/kimagure/items/c419ba740ac134a837a2>

This project lets you maintain package sets not as a blob of JSON, but rather as a Dhall source file, using the Dhall programming language. This then lets us use the full power of the Dhall language to build our package set, such as being able to merge a bunch of records to get our total package set, where we can merge new values into this record for new packages and override existing entries.

This also means that local overrides for packages are nothing but using a Dhall source file to import the parent package set from a remote url using Dhall's remote import feature:

```hs
    let mkPackage =
          https://raw.githubusercontent.com/spacchetti/spacchetti/140918/src/mkPackage.dhall

in  -- ...
```

There's a CLI that helps you do this without too much trouble: <https://github.com/spacchetti/spago>

Since I wrote this post, I've also written a large amount of documentation explaining how everything here works, from how Psc-Package itself works, why Dhall is used here, how to use this package set, and how to set up a project with local overrides for packages. See the guide here: <https://spacchetti.readthedocs.io/en/latest/>
