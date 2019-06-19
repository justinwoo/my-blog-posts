In 2019, we have some various easy ways to install PureScript and its tooling.

As a new user, you will only need to install the PureScript compiler and the build tool Spago.

## Installation

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

Then you can run `npm install --global purescript spago` to install the PureScript compiler and the Spago build tool.

### Nix

Clone Easy-PureScript-Nix from <https://github.com/justinwoo/easy-purescript-nix>

Then install PureScript and Spago to your path by using the derivations, e.g.

```
$ nix-env -if purs.nix

$ nix-env -if spago.nix
```

### Manually

Go to <https://github.com/purescript/purescript/releases> and <https://github.com/spacchetti/spago/releases>, and download the appropriate binaries for your platform. Then place these in a directory in your PATH.

## Hello World

Create a new directory and run `spago init`

```
$ mkdir new-ps-project-2019
$ cd new-ps-project-2019
$ spago init
```

Then you will have the initial Spago project setup. Then run `spago run` to see the project in action.

```
$ spago run
Installation complete.
Build succeeded.
🍝
```

There you have it, a working PureScript project ready for 2019.

## Links

* Example repo: <https://github.com/justinwoo/new-ps-project-2019>

* Spago: <https://github.com/spacchetti/spago>
