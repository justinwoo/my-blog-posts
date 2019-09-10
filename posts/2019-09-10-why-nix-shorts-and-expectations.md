# Why nix-shorts and expectations

Recently, I put together a small collection of short articles about Nix called nix-shorts: <https://github.com/justinwoo/nix-shorts>. In this post, I'll talk about why I put this together and what I expect from documentation in general.

## The problem with "introduction to Nix"

There exist some various "introduction to Nix" articles, blog posts, and talks, but they have some various problems:

### Irrelevant Nix implementation details

Examples include building derivations with `derivation` and making builders from scratch, evaluating expressions with `nix-instantiate`, discussion of GC roots, etc. Users introduced to Nix by these materials routinely do not understand why a derivation has inputs and outputs, and do not understand what will happen when `nix-build` is run on some derivation.

Imagine if we introduced people to Haskell by talking about GHC Generics or Template Haskell, but did not show users how to define and call functions.

### Lack of NixPkgs stdenv usage

New users to Nix will almost invariably consume and create derivations using the NixPkgs stdenv functions such as `mkDerivation`. Much of the existing material does not prepare users for this reality, so users almost always have to go about teaching themselves how to work with Nix "in the real world", rather than having this demonstrated to them.

### Demonstrations and examples do not exist

Demonstrations of very basic usages almost do not exist. While almost all users will clone NixPkgs and look through the implementations of various derivations, new users are heavily disadvantaged. One concrete example is how a new user might see a derivation definition that seems quite simple in its body, but they do not understand how named formal arguments and `callPackage` work for an expression.

### "Just read the Nix Pills"

For some reason, many people like to link to the Nix Pills, especially in useless internet chatrooms. Let me quote the Nix Pills for everyone:

> These articles are not a tutorial on using Nix. Instead, we're going to walk through the Nix system to understand the fundamentals.

Yes, they can be interesting, but most of the people who will point new users to the Nix Pills have rarely read the Nix Pills and do not understand the material themselves.

## Doing something about it

Unlike most lazy bastards on the internet, I decided to actually do the worst thing imaginable: put matters into my own hands. I started thinking about questions I get both online and offline from new and existing Nix/NixOS users, where the holes might be that prevent some connection of ideas that would enable them to solve their own problems and contribute to others' projects, and what details are just not put together for new users to truly get started. After collecting some of these topics, I set about writing about the ones I felt were most important for a user to get to work quickly.

Even after just a few weeks, I had something I could actually release that wouldn't just be some trickling of content.

## What I actually like about my own posts

### A clear topic

Instead of trying to cover all of the various details, I instead try to focus on a single working unit, such as a derivation.

### A definition

I try to actually give a fairly short definition of what the topic is. I do not try to go on some long explanation of philosophical discussion of merits.

### Demonstration, with commands and code

Everything you need to reproduce some result is explicitly provided, with all of the code, the commands, and the expected output. Without any one part of this, it is difficult for the user to know what the hell is supposed to happen as a result of typing in a bunch of shit.

### Explanations of unfamiliar bits

When talking about some specific topic, other details and unfamiliar parts will inevitably come up. Instead of ignoring them, I try to explicitly draw attention to them and provide a small explanation so that users will either understand enough of what is going on, or actively search and find out what that thing is.

### Links to other resources

Linking to external resources serves many purposes, but in particular, it allows the user to find out more about something without having to ask me directly or frantically search for the correct link. Remember, most of the Nix documentation is in the same layout and style, so a user can easily end up with Nix Pills, Nix User Manual, NixPkgs Contributors' Guide, or Nix Wiki from a given search link.

### Doesn't have to be read

People like to pretend like every word they write is a work of art, but almost nobody actually cares. They will skim whatever you write whether you like it or not. For example, people will mostly only read the headers in this post and possibly the first sentence of a given "paragraph". I accept this is normal behavior, and instead try to lay out what I write so that it does not have to be accessed in some progression. There is no drip-feeding of content here.

## This is how I read docs

I will also be lazy and use this short space to try to convince readers they should give the preceding parts a second skim before continuing on, as my points have been made.

## Anyone can write

For whatever reason, people regularly refuse to write, citing some various reasons:

* "I'm bad at writing" -- as if being "bad" ever stopped people, but also as if being "good" is a requirement
* "I don't want to maintain it" -- if it's relevant "only for one week", it still has a prime time of usability and provides some additional information someone can still use in the future. As long as what you publish to has dates, it doesn't matter. But if it doesn't include dates, you should consider changing what you're publishing on.
* "I'm too lazy" -- this is almost valid, but then I don't want to hear you complain about someone else putting effort into something in the future.
* "I don't know enough about it" -- writing about what parts you *do* know helps you reinforce what you know and provide information for people who don't yet know. I assume this has not stopped you from commenting on worldly matters or taking a job doing something you do not know fully.

Most importantly, everyone should write something down so that...

* you don't have to repeat yourself in every context
* other people can see what you have written without needing to bother you every time
* you can be a better person than someone who never contributes back

## Conclusion

I hope this has shown why I put together nix-shorts and what I see as problems about documentation that can be fixed, one person at a time.

## Links

* nix-shorts: <https://github.com/justinwoo/nix-shorts>
