This time, we'll take a total break from PureScript to talk about something everyone hates: setting up a way to make slides with latex-beamer.

Luckily, there are two big things I'll do to make this process way less painful: 1) I'll use Markdown and 2) I'll use nix to install the dependencies in a shell, so this is actually reproducible.

## "Easy Markdown to Beamer with Pandoc and Nix"

In this post, I go through how I used the combine function provided for the texlive derivation to stuff in all of the texlive packages I would need:

<https://qiita.com/kimagure/items/9d27015e12d4f22b53db>

With this setup, I no longer spend so much time with Google Slides on making my slides, and instead have more time to focus on and edit the content. Not that it was so bad to use Slides before when I could copy-paste from VSCode to get formatted code, but editing content and getting code laid out in Slides was so arduous before, where an average talk would take me over 10 hours to put together, whereas now I can easily get it done in 1 hour if I know what I want to talk about.
