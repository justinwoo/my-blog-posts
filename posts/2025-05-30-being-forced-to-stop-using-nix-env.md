# Being forced to stop using Nix-Env

Recently I was forced to stop using Nix-Env, because Nix >2.9.0 (newer than May 30, 2022) throws errors and makes evaluation fail: https://github.com/NixOS/nix/blame/586fa707fca207dbd12e49800691390249bdcd03/src/libexpr/get-drvs.cc#L153

The frustrating part is that I was quite happily using nix-env for both generating my ~/.nix-profile and installing ephemeral packages, but now I can't use this anymore. I admit, I am commenting on this three years after this commit, but it doesn't seem like the situation has improved much at all. When I was trying to install new packages into my Nix env, I simply got the error "this derivation has bad 'meta.outputsToInstall'" with nothing about which derivation was even the cause.

Having seen many comments over the years about how nix-env is satan incarnate, I decided to give up and change my setup.

## Replacing nix-env with... buildEnv

For my purposes, I was using ~/.nix-profile to house binaries and some other files I would need in its FHS, mostly /bin /etc /share. Then with nixpkgs buildEnv, building the env with the paths I needed.

```nix
{ pkgs ? import ./pinned.nix { } }:
let
  z = import ./z.nix { inherit pkgs; };

  packages = (
    with (pkgs.gitAndTools);
    [
      git
      git-extras
      hub
    ]
  ) ++ (with pkgs; [
    bash-completion
    colordiff
    direnv
    fd
    fzf
    gh
    jq
    neovim
    nix-bash-completions
    nixpkgs-fmt
    nodejs-18_x
    ripgrep
    shellcheck
    sqlite-interactive
    tig
    tmux
    watchexec
  ] ++ [
    z
  ]);

in
pkgs.buildEnv {
  name = "my-packages";
  paths = packages;
}
```

Then I built this with an output symlink which could then be used to update my PATH, bashrc, etc.

```shell
$ nix-build env.nix -j 20 --out-link result-env

$ ls -lha result-env/
total 0
dr-xr-xr-x      9 root  wheel    288B Jan  1  1970 .
drwxrwxr-t@ 11978 root  nixbld   374K May 28 05:55 ..
dr-xr-xr-x    124 root  wheel    3.9K Jan  1  1970 bin
dr-xr-xr-x     10 root  wheel    320B Jan  1  1970 etc
lrwxr-xr-x      1 root  wheel     66B Jan  1  1970 include -> /nix/store/hq1mb5pgy44x0ik4hnkzirb0b2nxrcra-nodejs-18.20.5/include
dr-xr-xr-x      7 root  wheel    224B Jan  1  1970 lib
lrwxr-xr-x      1 root  wheel     62B Jan  1  1970 libexec -> /nix/store/w5rggkxjd7z2cjk4fia9za7gb485pydw-git-2.47.0/libexec
lrwxr-xr-x      1 root  wheel     69B Jan  1  1970 rplugin.vim -> /nix/store/a7jnx8sq4nbk3jv605dyilg2x8c7n33d-neovim-0.10.2/rplugin.vim
dr-xr-xr-x     24 root  wheel    768B Jan  1  1970 share
```

This then did involve updating my config to replace ~/.nix-profile with ~/.dotfiles/nix/result-env, but that was okay.

I still don't have a very good way of dealing with ephemeral packages. My current thinking is to just make a host-specific env that I build and then symlink the bins into ~/.local/bin or something like so:

```bash
nix-build env.nix -j 20 --out-link result-env

mkdir -p "$HOME/.local/bin"

for file in result-env/bin/*; do
  filename=$(basename "$file")
  src=$(realpath "$file")

  ln -sf "$src" "$HOME/.local/bin/$filename"
done
```

But this is not that nice either.

## "Alternatives"

I have not spent much time on alternative methods that are suggested to me. Some of these are:

- nix profile
  - I do not use flakes and still do not plan to use them for a while yet.
- home manager
  - I don't like to use projects that are an entire framework, so I am not in the target audience for this tool.
- nix-shell for ephemeral packages
  - This I have a few opinions about, that the startup time of shells is unacceptable to me in either regular Nix or flakes.
  - With regular Nix, juggling channels or referencing specific nixpkgs versions in the nix-shell command is just too much work.
  - With flakes, I find that I am constantly running into fetches happening even when I specify nixpkgs versions, and that the evaluation is never correctly cached anyway, which defeats what flakes are supposed to promise.
- Using buildEnv to set ~/.nix-profile as intended (e.g. https://gist.github.com/lheckemann/402e61e8e53f136f239ecd8c17ab1deb)
  - Still runs into the problem with Nix CLI errors

It would be nice to at least use buildEnv to build the nix-profile that I want, but this doesn't seem to have any actual support. If anything, when I attempt to use it, I run into errors from the Nix CLI and eager evaluation of nixpkgs, making it impossible to do anything.
