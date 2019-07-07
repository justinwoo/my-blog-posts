# PureScript package management explained

In this post, I will explain how the PureScript package management solutions work and why they exist.

## Introduction

Package management solutions in PureScript are much more lightweight than what most people are used to, in that they all operate in roughly the same way: glorified Git fetch. This actually works because of two main factors:

* There are no special install hooks.

If you use npm, you are almost certainly installing packages that require installation hooks. These often end up with you downloading pre-compiled binaries for your platform or compiling some C++ that hooks into V8. PureScript dependencies don't ever have any hooks performed.

* You source all of your dependencies' source files to the compiler

This is not very much of a problem in PureScript since our compiler is decently fast enough, considering the amount of work done. A compiler that does less work *should* always be faster, so there's no surprises if other languages have faster or slower compilers. This is still an active area of the compiler's development, but it doesn't really matter for users.

So does this mean you don't actually need a package manager? Pretty much. If you wanted, you could have a text file of links to tarballs to be extracted to a directory, and that would work.

## Psc-Package

Psc-Package works by using package sets to install packages. What is a package set? We have written about this here: <https://psc-package.readthedocs.io/en/latest/intro.html#what-is-a-package-set>

In short, it is a Git repository *separate* of the Psc-Package repository, which contains some `packages.json` of packages at specific versions. This `packages.json` is the actual package **set**.

This is a **set** because there is only **one** entry of a package at a given **version**. There is no such thing as having packages at multiple versions, because you would then be compiling with duplicate definitions of a given module. To do so would be to run into the "Dreaded Diamond Dependency Problem" <https://www.well-typed.com/blog/2008/04/the-dreaded-diamond-dependency-problem/>. You can read more about why PureScript dependencies are installed in a flat matter here: <https://harry.garrood.me/blog/purescript-why-bower/>

Of course, people often opt to using the "default" package set from <https://github.com/purescript/package-sets/>. But you may find that this does not contain packages you want or does not have some updates that you would like for it to have. In many cases, you may choose to use my package set instead: <https://github.com/justinwoo/spacchetti>

### Spacchetti

Spacchetti is a project that came about in June 2018, to solve "how can package sets be made easier to maintain and edit?" I started putting this together because of a number of reasons:

1. Editing a giant JSON file is not very fun.
2. I want to combine information from multiple different "groups", so that I can separate and then merge packages from various different users/organizations.
3. I want to be able to override existing package definitions easily and programmatically, with some typed tool.

This is why Spacchetti came about using Dhall. You can read more about Spacchetti on the docs page: <https://spacchetti.readthedocs.io/>

I also wrote about this previously: <https://qiita.com/kimagure/items/c419ba740ac134a837a2>

## Spago

So Psc-Package has one obvious problem: you cannot programmatically prepare your package set. This is mostly solved by Spacchetti, but Psc-Package cannot consume the Dhall package set by itself.

Thus, we have this "Spago" tool that was born out of a CLI originally built for using Spacchetti with Psc-Package, into what can essentially replace both Psc-Package and Pulp. You can almost think of this project as Stack, but without the YAML mess.

There is a lot of information written by Fabrizio in the README of the project: <https://github.com/spacchetti/spago>

## Bower

Traditional project management with version resolution conflict problems. I don't use this other than to manage dependencies for libraries.

Please read Harry's post about why PureScript libraries and projects might use Bower: <https://harry.garrood.me/blog/purescript-why-bower/>

I do not use Bower to manage dependencies for projects.

## Conclusion

There is no real conclusion to this post which is just an information dump. If you want active support, **use Spago**. If you want to use some lightweight solutions and really like DIY, then **use either Psc-Package or Spago**. If you really don't care about anything I've written here and you're okay with using unpopular technology, **use Bower**.

If you still don't know what to do, start from here: <https://github.com/justinwoo/spacchetti-react-basic-starter>