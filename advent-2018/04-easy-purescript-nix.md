Since the beginning of time, people complained about this combination of obscure technologies of PureScript with Nix. "The derivation on nixpkgs doesn't work!" "This doesn't build!" "There are missing deps!" "Why is the nixpkgs derivation for Psc-Package not updated?" And while many users may consume Nix packages, they often do not understand how to fix broken derivations, let alone know what they even are. Well, it's not entirely their fault for not knowing though.

So instead of trying to tackle the incredibly hard problem of how to get derivations on Nixpkgs fixed and working "correctly" (whatever that means), I started looking at the problem from the other side: "Why don't I just get to work using Nix to make my own damn derivations for PureScript tooling? It's not like I even like Nixpkgs so much anyway."

Famous last words.

## "Using PureScript easily with Nix"

In this post, I talk about how I set up an easy way to get PureScript tooling installed to use from a project by using nix derivations. This approach ultimately lets us use PureScript tooling by simply running `nix-shell` in the root of a project, and also allows for being able to install tooling to our global environment by feeding an expression into `nix-env`.

<https://qiita.com/kimagure/items/de2a4ff45dd8fe8be4b1>

Since I wrote this post, some others, especially my friend Pekka in town, helped get the various expressions fixed up so that things work properly also on NixOS. Definitely check this project out if you want to easily and reliably get PureScript (and related) tooling installed and prepared easily:

<https://github.com/justinwoo/easy-purescript-nix>

And so while the Nix language is strange and doesn't provide the best tools, a combination of `nix repl` and tools like `nix-show-derivation` should help you chase down issues you run into.
