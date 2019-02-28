---
title: Setting up PureScript in December 2018
tags: purescript
author: kimagure
slide: false
---
**Updated December 10th, 2018. Original title: "Setting up PureScript in March 2018"**

People ask me how to set up PureScript often, and while the majority of the people I talk to are already experienced with JS tooling (both in web and node), I've also run into plenty who haven't been exposed to either.

So in order to try to answer hundreds of people at once, I'll write some kind of "opinionated piece" that goes over some normal setup things you'll probably want.

And no, we're not going to use Bower, [even though it works](http://harry.garrood.me/blog/purescript-why-bower/), because even I don't use Bower anymore, at home or at work.

## Install Node >= 10

Weird as it is, I have to get people to install Node >= 10 since many have outdated Node installations (often from apt), so please go install it as you will at least need it to run Pulp and your PureScript-Node programs: <https://nodejs.org/en/download/>

*You CAN choose to not use Node at all when using PureScript. You'll just not be able to use Pulp (not a big deal) and you won't be able to run PureScript on Node (well, yeah)*

## Configure NPM

The default npm config you get from most installations will give you some wonky setup where the workaround ends up being to `chown` directories or run npm as `sudo`. **Please don't ever do these.** What you actually want is to set your `.npmrc` file to use a prefix:

```
prefix=~/.npm
```

Or run `npm set prefix ~/.npm`

## Set your PATH

You'll then probably want to add the `bin` directory to your PATH:

```
export PATH="$HOME/.npm/bin:$PATH"
```

This is fairly optional, but you will probably want to have your project installed node_modules `.bin` in your PATH also:

```
export PATH="./node_modules/.bin:$PATH"
```

## Install PureScript, Pulp, Psc-Package via NPM

**Update Dec 10th, 2018: You can install psc-package from NPM now! See <https://www.npmjs.com/package/psc-package>**

Unless you really dislike npm, unless you do work on the compiler or psc-package, you will probably not need much else than npm installed programs. So install these globally at first, and then choose whether or not you want to install these locally later:

```
npm i -g purescript pulp psc-package
```

Check that these work by trying some commands:

```
pulp --version
psc-package --version
```

Just to save yourself the pain, you probably want to do something like `alias pp=psc-package`, but this is up to you.

## Install some editor plugins

I use [Spacemacs](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/purescript), [Vim/Neovim](https://github.com/FrigoEU/psc-ide-vim), and [VSCode](https://github.com/purescript/documentation/blob/master/ecosystem/Editor-and-tool-support.md). Using Atom or Sublime? There's notes on those too: <https://github.com/purescript/documentation/blob/master/ecosystem/Editor-and-tool-support.md>

## Now try a hello world

Make a dir and cd into it like usual.

```
mkdir hello; cd hello
```

Important note here: Pulp currently still assumes you want to use Bower, but we don't. So you probably will want to alias the psc-package mode Pulp like so: `alias p=pulp --psc-package`. Now init the project and try running it.

```
pulp --psc-package init; pulp run
```

If it doesn't flood you with compile messages and "Hello sailor!", then we have a problem. Otherwise, you have everything set up now. Now you can go on and write some web applications with [React-Basic](https://github.com/lumihq/purescript-react-basic), [Halogen](https://github.com/slamdata/purescript-halogen) or Node applications in whatever way you choose. This might be a good time for you to join the [FP slack](https://fpchat-invite.herokuapp.com/) and ask people on #purescript and #purescript-beginners what kinds of things you should try doing. Sky's the limit!

## Conclusion

Hopefully this has shown you that you can get started with PureScript in 2018 without Bower and get on your way to taking advantage of anything that exists in PureScript and JavaScript. Have fun!

At least, even if you choose not to use PureScript, now you can smugly correct people when they complain that "PureScript is tied to Bower" by telling them about how you could very well use Psc-Package or any custom approach to feeding source globs to the PureScript compiler.

## Changes Dec 10th, 2018

I made some updates to this posts on Dec 10th, 2018. The changes are the following:

* I can now recommend installing psc-package from npm for beginners, as I maintain the package and keep it updated, and it works on Windows. See other methods of installation in the docs: <https://psc-package.readthedocs.io/en/latest/installation.html>
* Added link to React-Basic, as it is a very readily usable library for newcomers and experienced users alike.
* Changed NodeJS recommendation from 8 to 10.

## Links

* PureScript: http://purescript.org
* FP Slack: https://fpchat-invite.herokuapp.com
* Psc-Package: https://github.com/purescript/psc-package
* PureScript by Example: https://leanpub.com/purescript/read
* Getting Started with PureScript: https://github.com/purescript/documentation/blob/master/guides/Getting-Started.md

