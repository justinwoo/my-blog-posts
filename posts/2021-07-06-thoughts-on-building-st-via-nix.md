# Thoughts on building st via Nix

For quite a while I was using alacritty, but I eventually got annoyed about it because I was not really in the target demographic:

* I use computers with low power GPUs, so even opening multiple alacritty instances would cause freezing and visual lag
* I got tired of trying to keep up with their configuration changes over time
* East-Asian characters always had some various problems through the years, that I mostly tried to ignore
* Extra features like "vim keybindings" didn't really make sense for me as I do my main work in tmux sessions

So my friend Jonas asked me why I even bothered using alacritty anymore. He suggested instead that I should try his st terminal setup.

## On patches for functionality

st works by simply having contributors publish their patches on some version of st to add functionality.

I do not really have an opinion on this if this is good or bad.

## The basic setup

The Nixpkgs st derivation does most of the plumbing for you, so all you need to do is provide the diffs that you want to apply.

```nix
{ pkgs ? import <nixpkgs> { } }:

pkgs.st.overrideAttrs (old: {
  patches = old.patches ++ [
    (builtins.fetchurl {
      url = "https://st.suckless.org/patches/scrollback/st-scrollback-0.8.4.diff";
      sha256 = "0i0fav13sxnsydpllny26139gnzai66222502cplh18iy5fir3j1";
    })

    ./url.diff
    ./invert.diff
    ./term.diff
    ./solarized-accented.diff
    ./font.diff
    ./shortcuts.diff
  ];
})
```

## Problem

The most obvious problem I have with this is that diffs don't merge very well. The most common problem being that any changes to the `shortcuts` variable in `config.def.h` will cause conflicts.

If you choose to manage a git repo for your fork of st, then your process would be to fix merge conflicts. However, this doesn't work if you would actually like to manage a feature matrix, where you add and remove patches you use over time.

However, in this patches-driven method, I cannot see how I would work with this other than to download all the patches directly the modify them so that

* I would have to remove all changes to `shortcuts` in the upstream patch I downloaded
* I would have to consolidate all of the `shortcuts` changes to a single diff

And so I have done like so:

```diff
diff --git a/config.def.h b/config.def.h
index 3eabc86..10f39ec 100644
--- a/config.def.h
+++ b/config.def.h
@@ -191,8 +191,10 @@ static Shortcut shortcuts[] = {
 	{ ControlMask,          XK_Print,       toggleprinter,  {.i =  0} },
 	{ ShiftMask,            XK_Print,       printscreen,    {.i =  0} },
 	{ XK_ANY_MOD,           XK_Print,       printsel,       {.i =  0} },
-	{ TERMMOD,              XK_Prior,       zoom,           {.f = +1} },
-	{ TERMMOD,              XK_Next,        zoom,           {.f = -1} },
-	{ TERMMOD,              XK_Home,        zoomreset,      {.f =  0} },
+	{ ControlMask,          XK_equal,       zoom,           {.f = +3} },
+	{ ControlMask,          XK_minus,       zoom,           {.f = -3} },
+	{ ControlMask,          XK_0,           zoomreset,      {.f =  0} }, // number zero
+	{ TERMMOD,              XK_O,           copyurl,        {.i =  0} }, // letter 'o'
+	{ TERMMOD,              XK_X,           invert,         { }       },
 	{ TERMMOD,              XK_C,           clipcopy,       {.i =  0} },
 	{ TERMMOD,              XK_V,           clippaste,      {.i =  0} },
 	{ TERMMOD,              XK_Y,           selpaste,       {.i =  0} },
```

## My question

So this is what I'm stuck on, that it seems like there should be a better way to work with this, that I could convert all of these diffs into some proper patch format that I could then actually combine and convert back into a single diff.

Or maybe there is another way. I would like for someone to create their own example and show me how they think this should work.

## Conclusion

This setup for st works quite well for me to get a terminal that basically does all the things I need it to do. But I do hope that someone has some ideas on how working with this could be improved in some way.

## Links

* st https://st.suckless.org/
* my-st https://github.com/justinwoo/my-st
* nixpkgs st https://github.com/NixOS/nixpkgs/blob/976791a768ae57b66718e1ba52c86d2e66462187/pkgs/applications/terminal-emulators/st/default.nix
