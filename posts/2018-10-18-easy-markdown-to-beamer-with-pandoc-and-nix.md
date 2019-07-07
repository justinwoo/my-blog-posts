# Easy Markdown to Beamer with Pandoc and Nix

For the past couple of years, I've been trying to use Google Slides to prepare presentations, copying formatted text from VSCode to get highlighted and formatted code in my slides. However, since preparing slides in Google Slides ends up being mostly work on trying to get the layout to play nice, I've decided to stop using Google Slides and opt for a simpler option: beamer.

However, it's quite difficult to get enough information about how to do this, so I have put together a repository that can be used as a reference: <https://github.com/justinwoo/easy-markdown-beamer-pandoc>

## The parts

* Markdown - it's easy enough to use
* Beamer - how else do I readily get PDFs for slides
* Pandoc - I need markdown to beamer to PDF
* Nix - I need to be able to actually install the same tools across computers

I run Ubuntu on my computers (I use three computers regularly) since I don't want to deal with any drivers and generally don't mind packages installed through apt. However, getting a consistent texlive installation between my computers that didn't cost 10GB+ proved to be quite tough, and the time factor was also painful -- even though I have a gigabit connection at home, I can't download packages at high speeds, likely just the problem of living in Finland. The only things that download quickly for me are torrents and Steam games.

## Setup

### `default.nix`

First, I have a `default.nix` file to define what all I need installed. To get a texlive installation with the correct dependencies I need, I end up defining a custom set:

```hs
let
  pkgs = import <nixpkgs> {};

  texlive = pkgs.texlive.combine {
    inherit (pkgs.texlive)
    scheme-small
    noto
    mweights
    cm-super
    cmbright
    fontaxes
    beamer;
  };
in {
  slides = pkgs.stdenv.mkDerivation {
    name = "slides";
    src = ./.;

    buildInputs = [
      texlive
      pkgs.pandoc
      pkgs.watchexec
    ];
  };
}
```

So in my set, I use `scheme-small` for a base installation, the `noto` font, the `cmbright` font, `beamer`, and some other utilities required to build my document. This might not be a complete set, but it's what I need to get started quickly. For the full details, see the pinned pure nix build in the repository.

For more information about custom sets for texlive, see <https://nixos.org/nixpkgs/manual/#sec-language-texlive>

Then for my other build inputs, I declare that I will also need Pandoc and WatchExec for executing commands on file changes.

### `Makefile`

I set up a `Makefile` for the basic things I want to be able to run easily:

```
default:
	nix-shell --run 'make slides'
slides:
	pandoc -t beamer slides.md -o slides.pdf
watch:
	nix-shell --run 'watchexec -e md make'
repl:
	nix repl '<nixpkgs>'
```

The tasks are self-explanatory, but the repl allows me to look at packages and their contents, so the top-level of the repl on startup has completion for `texlive` and various things under `texlive.[name]`, which helps me fix anything that breaks as a result of changing options for pandoc/beamer.

## Usage

Finally, I have my slides in a markdown file. Importantly, you can use a yaml section at the beginning to set title, author, date, theme, colortheme, fontfamily, etc. along with `header-includes` to include some latex into the header.

```md
---
title: Easy Markdown to Beamer with Pandoc
subtitle: Simple things nobody tells you how to do
author: Justin Woo
date: 2018 onwards
theme: Madrid
colortheme: dolphin
fontfamily: noto-sans
header-includes:
- \usepackage{cmbright}
fontsize: 10pt
---

# Code demo

Here's some code:

``hs
data Maybe a = Just a | Nothing
``

Here's a centered image of a ratio of textwidth:

\begin{center}
  \includegraphics[width=0.4\textwidth]{./image.png}
\end{center}
```

In my example here, I'm able to simply include a code block, and also include a latex block to include an image that is 40% of the page width.

## Result

The result looks like this when we run `make`:

![](https://raw.githubusercontent.com/justinwoo/easy-markdown-beamer-pandoc/master/preview-0.png)

![](https://raw.githubusercontent.com/justinwoo/easy-markdown-beamer-pandoc/master/preview-1.png)

## Conclusion

Hopefully this has shown you that working with markdown+beamer+pandoc for making slides can be made fairly easy and reproducible by using the Nix package manager, even if it's a bit hard to find much documentation about it.