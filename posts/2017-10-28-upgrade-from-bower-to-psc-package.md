---
title: Upgrade from Bower to Psc-Package
tags: purescript
author: kimagure
slide: false
---
Even though Bower has worked and continues to work for PureScript, most end user applications benefit greatly by having a static set of dependencies installed. This post will go over the relatively simple steps it takes to upgrade an existing project using Bower to use Psc-Package instead.

## Why not Bower?

Let's review some reasons why you might not want Bower:

* You probably don't need or even want the dependency resolution, since you want a set of dependencies that work when installed flat. Ironically, this doesn't work at all for front-end JavaScript anymore as many have switched to using CommonJS modules for everything, necessitating nested module installations to have correctly working modules.
* You might want to use a package set to guarantee that all packages you install will work together. Furthermore, you might want to add packages at specific versions that you know work with the rest of the modules in your set.
* Everyone will complain at you forever if you keep using it. Not a very technical reason, but who wants to hear the same complaints day in, day out?

Well, these all seem like pretty normal reasons to not use Bower, right? Let's get started.

## Step 1: Get psc-package

Thanks to the hard work of Hardy Jones, you can now install `psc-package` through npm. Just `npm install -g psc-package` and it'll work!

## Step 2: Init psc-package

Go into your project and run `psc-package init` to initialize the project with a `psc-package.json`. You'll get a file that looks like this:

```json
{
    "name": "your-project",
    "set": "purescript-0.11.6",
    "source": "https://github.com/purescript/package-sets.git",
    "depends": [
        "prelude"
    ]
}
```

You might want to choose a different package set to use here. In my case, I have my own package set in my repo with tagged releases, so I have the lines `set` and `source` edited like so:

```json
    "set": "aff-4.0-27-Oct-2017",
    "source": "https://github.com/justinwoo/package-sets.git",
```

## Step 3: Copy your bower.json dependencies, really

One caveat: psc-package packages don't use `purescript-` as a prefix and don't specify the version in the dependency list. One easy way for you to get your dependencies is to do the following:

1. Go to `bower.json`
2. Copy the list of dependencies and paste it into a new buffer
3. If you use vim/spacemacs, first you probably want to use `:%s/purescript-//` to clear `purescript-`, and then use `:%norm f:Da,` to use normal mode to find `:`, delete to the end of the line, and append `,`. You probably want to hit `x` to delete the last comma, and then use `:%y+` to copy the buffer into your clipboard.
4. Take your dependencies and paste them into your `psc-package.json` file under `depends` like so:

```json
    "depends": [
        "node-he",
        "lenient-html-parser",
        "node-process",
        "milkis",
        "simple-json",
        "foreign-generic",
        "node-sqlite3",
        "node-fs-aff",
        "node-child-process",
        "affjax",
        "aff",
        "console",
        "prelude"
    ]
```

## Step 4: Run the update to install everything

Run `psc-package update` and it'll figure out what all you need and install them. If you have stuff that's not in here, then you'll have to add stuff to your package set, but thankfully that's not too much work. I'll cover this at the end.

Otherwise, everything should now have been correctly installed and all.

## Step 5: Delete the bower

Run `rm -rf bower_components; rm bower.json`. Congratulations, you are now Bower-free!

## Checking things work

At this point, you should be able to run all `pulp` commands without any prefixes whatsoever. If `pulp build` doesn't work, then you either need to update your code, find a package set that suits what you're using, or actually go out and ask for help from Twitter #purescript/[@jusrin00](https://twitter.com/jusrin00) or #puresript on the FP Slack (https://fpchat-invite.herokuapp.com/).

## Adding more things in the future

If you need to add more dependencies, then nothing more than `psc-package install your-dependency` should be required.

## Conclusion

Hopefully this has shown you some concrete ways to move from Bower to Psc-Package. Let me know on Twitter or the FP Slack if you need any help!

Tl;dr this blog post in a tweet: https://twitter.com/jusrin00/status/923811954262757376

## Links

* https://github.com/purescript/psc-package
* https://github.com/justinwoo/package-sets
* My YouTube video on new project setup with Pulp: https://www.youtube.com/watch?v=bNavBHmIknQ

### Adding things to the package set

At this point, you probably want to talk to someone, but overall there are only these steps:

1. Fork `purescript/package-sets`
2. Edit `package.json` in the repo to copy an existing spec and change the things as you need, like version, dependencies, etc.
3. Run `travis.sh` and break when it starts verifying the set, as you probably don't want to spend all the time verifying the full set.
4. Run `psc-package verify your-new-package` to verify that package.
5. Push the commit in a branch or a tag, whichever you wish.
6. Start using this from your projects!

