# Some backported PureScript changes for my own uses

## What was backported, primarily

Recently, I backported some changes from purescript/master to 0.13.6, so that I could actually target the main problem my team and I have at work using PureScript: the IDE server was painful to use due to how slow it was.

So Christoph made a fix for this fairly recently, where he found that one of the greatest time sinks of the PureScript IDE server was JSON parsing. Specifically, externs files made for very slow parsing, as many modules that were built would result in JSON files ranging from 100+ KB to 10+ MB. So much so, that many of my PureScript projects, using a good number of dependencies, would require 30+ seconds to actually load.

You can read more about the fix here, where the JSON output was replaced with CBOR: https://github.com/purescript/purescript/pull/3841

So while the file sizes for the CBOR output do not differ significantly for me, my projects went from 30+ seconds to 3+ seconds to load the IDE server. This means that now when I edit files in my project, I can actually load up the new module information of my project in a few seconds to get back to work, rather than to have to eat another 30+ second pause. Amazing. Thanks Christoph.

## What was not

However, there are some huge changes that have been merged to PureScript before this, being the Coercible types and Polykinds implementations. While people might find either or both of these very useful, these are not the changes that I'm struggling to find when working with PureScript. I have made some jokes that working with polykinds would be better than what limitations we have now (see https://twitter.com/jusrin00/status/968471589812670465, https://twitter.com/jusrin00/status/924023047526518784), but at the end of the day, what has been the biggest pain for my team has been speed and reliability of the PureScript IDE tools and compiler.

These huge changes seem to prevent any kind of release happening anytime soon, and that's understandable. Fundamental changes to how PureScript works are not going to be easy to merge and release, and come with their own share of problems. I am not here to demand some PureScript 0.14 release.

## Unresolved things that bother me

One major problem I still have left is that the PureScript compiler is not good at coming up with what should build and how. Because I worked with cached builds in CI, I have many situations in CI where stale output that should have been rebuilt does not, leading to type errors and other failures that are invalid and are solved only by removing the existing compiler output.

This problem seems to arise entirely from the compiler's use of timestamps to determine if output and input are mismatched, and how they are mismatched. Clearly, it seems the comparison of the inputs and output timestamps fails somewhere involving transitive inputs, so that my project very easily enters a half-built state. I do not blame anyone for not wanting to look that much into this problem, since it is painful and complex, but it is something that I hope will be addressed in the future.

However, I have ended up with my own solution to this problem by choosing to build the PureScript part of my project as a Nix derivation of two steps: one derivation to build my dependencies, and one derivation to use the output from the dependencies derivation to add on output from my project. I do not plan to write about how I did this, because there is nothing more than implementation here. If you would really like to do this for your own project, please contact me to arrange consulting.

## Where this is

On my GitHub fork of purescript/purescript I now have a branch here with my backported changes: https://github.com/justinwoo/purescript/commits/0.13.7

This fork is quite literally some few rebased commits on top of 0.13.6, with many exclusions of commits 0.13.6~purescript/master.

If you think what I have written here sounds interesting, feel free to try checking out this branch and building it for your own uses. This should require nothing more than running `stack install` and ensuring you have ncurses5 and some other dependencies available on your system. I've also included a Nix shell which will set `LIBRARY_PATH` with libraries that I have found I need to build the compiler on my machine:

```nix
{ pkgs ? import ./pinned.nix {} }:
let
  dynamic-linker = pkgs.stdenv.cc.bintools.dynamicLinker;
  buildInputs = [ pkgs.gmp pkgs.zlib pkgs.ncurses5 pkgs.stdenv.cc.cc.lib ];
  libPath = pkgs.lib.makeLibraryPath buildInputs;
in
pkgs.mkShell {
  buildInputs = buildInputs;
  shellHook = ''
    export LIBRARY_PATH=${libPath}
  '';
}
```

Otherwise, if you like to run binaries from the internet, you can get the binaries that I have built and acquired from the release pages: https://github.com/justinwoo/purescript/releases

I leave it as an exercise to the reader to implement a Nix derivation that consumes these binaries by referencing easy-purescript-nix here: https://github.com/justinwoo/easy-purescript-nix/blob/d4879bfd2b595d7fbd37da1a7bea5d0361975eb3/purs/mkPursDerivation.nix

## Conclusion

I have been using this result now with my team for a short while and have found that this works quite well.

However, do not mistake that this post is some declaration of a fork of PureScript. I do not currently have any interest in maintaining a fork or trying to work with any forks in general. And to clear up any misunderstandings: I have not been maintaining the PureScript compiler nor do I plan to, and I have maintained a largely detached life of using PureScript from the rest of the "PureScript community". Even then, I have largely stopped doing anything related to PureScript outside of work for personal reasons.

If you try out this fork and really like it for various reasons, then please say something about it in an appropriate way. If you don't like this fork for some reason, then please consider writing about it on a napkin and throw the napkin in the bin. Thanks.

## Links

* My branch: https://github.com/justinwoo/purescript/commits/0.13.7
* Binaries: https://github.com/justinwoo/purescript/releases
