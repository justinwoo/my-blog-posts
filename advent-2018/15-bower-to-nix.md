Even though nobody needs to suffer installing their project dependencies with Bower anymore, we still have some people with projects started in Bower from the past couple of years who might not have changed. Worse, sometimes we have users who have a project started with Bower, not knowing that they have much better options to work with.

A long time ago, I wrote about how to upgrade from Bower to Psc-Package: <https://qiita.com/kimagure/items/0d9354900d7a7dbd3864>

And while this is enough to work with our projects in a pretty modern way, Psc-Package itself is a very simple tool that doesn't try to introduce much. If we want a robust way to verify the contents of dependencies we pull down and store them to be shared between multiple installations, we're going to have to come up with good ways to do this.

## "Upgrade from Bower to Nix with Psc-Package2Nix"

In this article, I wrote about how to upgrade a project using Bower to Nix via Psc-Package2Nix:

<https://qiita.com/kimagure/items/aec640d0047d08d2ce90>

With this, we can use a simple `psc-package.json` file to specify the package set definitions and direct dependencies just like with regular Psc-Package usage, but then we can use Psc-Package2Nix to get a set of derivations we can use to install our dependencies.
