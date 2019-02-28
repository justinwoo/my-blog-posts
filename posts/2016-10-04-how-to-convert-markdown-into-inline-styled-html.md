---
title: How to convert Markdown into inline-styled HTML
tags: Markdown HTML difficultToSearch
author: kimagure
slide: false
---
I needed to get some Markdown into inline-styled HTML for my work's blog today, was surprised I couldn't find any straightforward guides. Well folks, here we are:

1. generate your CSS with Pygmentize
2. pandoc your markdown with that css
3. use [premailer](https://pypi.python.org/pypi/premailer) or something to inline css styles
4. remove class attributes if your CMS removes all attributes when there are `class` attributes
5. put it in your pasteboard

```bash
pygmentize -S default -f html > default.css
pandoc input.md > intermediate.html -c default.css
python -m premailer -f intermediate.html -o output.html
sed -i -E 's/class="[^"]*"//g' output.html
cat output.html | pbcopy
```

…and that’s how you get from normal Markdown to inlined-styled HTML for your CMS or whatever the hell.

Thanks to my coworker and [@rightfold](https://twitter.com/rightfold) for the tip.

As a bash script gist here:

```bash
#!/bin/bash
if [ "$#" -ne 1 ]
then
  echo "Supply an input markdown file for this script to work."
  exit 1
fi
input="$1"
output="$1-output.html"
rm $output*
pygmentize -S default -f html > default.css
pandoc $1 > intermediate.html -c default.css
python -m premailer -f intermediate.html -o $output
sed -i -E 's/class="[^"]*" //g' $output
rm intermediate.html
cat $output | pbcopy
```

