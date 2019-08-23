Psc-Package is a tool for PureScript based on package sets -- that a given package should only have one version. This means that for any package that depends on `purescript-effect`, they will all only use `purescript-effect` at the given version, and will be validated to work with it when the package set is verified for consistency.

And while you could read some literature on why Bower was used as a default for PureScript projects (such as in Harry's blog here: <https://harry.garrood.me/blog/purescript-why-bower/>), it would be more productive to talk about how Psc-Package works and how you can use it today.

## "Making a new library and using it in your own Psc-Package set"

In this post, I go through how you can have your own fork of package sets that you can update with your packages.

<https://qiita.com/kimagure/items/c37b228e80318d4158f0>

Since I wrote this post, I've written some dependencies for an alternative package set solution I have called "Spacchetti" (as in, a *packet* of *spaghetti*). In these guides, I have a quick explanation of what Psc-Package is and how it uses package sets:

<https://spacchetti.readthedocs.io/en/latest/intro.html>

In a future post, I'll go more into how Spacchetti works by utilizing a programming language called Dhall to generate package set JSON.

You may also be interested in this post about upgrading a Bower project to Psc-Package: <https://qiita.com/kimagure/items/0d9354900d7a7dbd3864>

You might also want to read about how you can implement your own Psc-Package: <https://qiita.com/kimagure/items/625070775da70b37b67e>
