# Installing PureScript in 2020

In 2020, we have some various ways to install PureScript and its tooling. This is a summary of the ways I have worked with PureScript with other people, at work and otherwise over four years. These are just one of many opinions of how to work with PureScript, but maybe one of few published ones.

## Installation

As a new user, you will probably want to install PureScript, Pulp, and your choice of various methods to manage PureScript dependencies.

### npm

First, please make sure you have

* Node 10.x or greater
* npm prefix set to a folder you own

To set the npm prefix, you should either run

```
$ npm set prefix ~/.npm
```

Or set the file `~/.npmrc` to have `prefix=/home/your-user/.npm`.

**Do not ever run npm with sudo.**

Then you can run `npm install --global purescript pulp` to install the PureScript compiler and the Pulp build tool globally. You may also choose to install one or both of these in your project specifically.

### Nix

Get Easy-PureScript-Nix from <https://github.com/justinwoo/easy-purescript-nix>

Then install PureScript and other tools as you need. If you are not familiar with how to use Nix, you might refer to my Nix guide here: <https://github.com/justinwoo/nix-shorts>

For the most part, if you are a Nix user, you should import Easy-PureScript-Nix into your project and add it to your Nix Shell. See the Easy-PureScript-Nix README for more information.

### Manually

Go to <https://github.com/purescript/purescript/releases> and download the appropriate binaries for your platform. Then place these in a directory in your PATH.

### Package management

Then please make your own decisions on trying to use Psc-Package or Spago. The former is a stable project that I have used for many years. The latter is a newer project that you may find helpful. If you have questions about using Psc-Package, I can answer your questions directly for the most part.

If you simply do not want to use a PureScript package manager and you use Nix, there are some various solutions I have made available for everyone to use. You might be interested in reading my posts about using PureScript on Nix: https://github.com/justinwoo/my-blog-posts/blob/master/posts/2020-05-31-working-with-purescript-package-sets-with-just-nix.md, https://github.com/justinwoo/my-blog-posts/blob/master/posts/2020-06-27-purescript-on-nix-without-dependency-codegen.md

If you want to know why PureScript does not use npm, please read this post from Harry Garrood: <https://harry.garrood.me/blog/purescript-why-bower>

### Editor tooling

I personally only use [psc-ide-emacs](https://github.com/purescript-emacs/psc-ide-emacs) and the [VSCode PureScript tooling](https://github.com/nwolverson/vscode-ide-purescript). For other editors and setups, you may want to search through the PureScript Discourse <https://discourse.purescript.org/>.

## Personal notes

Most information here stays the same as my posts from 2019 and 2018: https://github.com/justinwoo/my-blog-posts/blob/master/posts/2018-03-20-setting-up-purescript-in-march-2018.md, https://github.com/justinwoo/my-blog-posts/blob/master/posts/2019-06-20-installing-purescript-2019.md

I am now personally only using PureScript with Nix and Psc-Package, with some usage of Pulp when using Psc-Package. This has been what has caused the least problems for me and the people who work with me and ask me for help. However, the main thing that is nice about PureScript is being able to choose what you want, so please evaluate any and all other solutions whenever you can.

-----

Now that you've scrolled through this post, why not start looking at what you can do with PureScript? Maybe you might be interested in getting started with JSON: <https://purescript-simple-json.readthedocs.io/en/latest/quickstart.html>

## Links

* Example repo: <https://github.com/justinwoo/new-ps-project-2020>

* Pulp: <https://github.com/purescript-contrib/pulp>

* Psc-Package: <https://github.com/purescript/psc-package>

* Spago: <https://github.com/purescript/spago>
