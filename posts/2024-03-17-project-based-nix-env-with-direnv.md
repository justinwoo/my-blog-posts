# Project-based Nix env with direnv

Like many users, I've been using nix-shell in various projects over the years. This has been nice for ensuring that everyone contributing to a project can have the same toolchain and whatnot as you'd expect from using a nix shell.

However, there are some various problems I have with nix-shells:

* Evaluation time: the bigger and more complex your project becomes, the longer it takes to evaluate nix-shells and start them up.
* Startup time on Mac OS: everything involving touching the file system seems to take a huge hit on Mac OS, and Nix is no exception. A nix-shell taking 1s on Linux can easily take 4s+ on Mac OS. This means that you constantly run into friction when wanting to create new terminal sessions.
* Bash: I don't believe it's possible to use non-bash shells still, which isn't a problem for me, but many users only have zsh or other shells set up.

So in reality, all that I'm looking for from a nix-shell is to provide some paths that are to be consumed in my project, where I'm mostly only looking to source binaries to my PATH and maybe add some shared libraries to RPATH and such.

## buildEnv

In nixpkgs, we have the buildEnv function that provides for a way to build an environment like with Nix profiles. However, documentation is a bit light, requiring you to look at the implementation of buildEnv in nixpkgs: <https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/buildenv/default.nix>. Also of note is some docs in the nixpkgs manual showing how to use buildEnv: <https://nixos.org/manual/nixpkgs/stable/#sec-building-environment>.

We can take what's written here and apply it to our uses, where we will build an environment that is used within our project and contains what we need.

First, starting off with a pinned nixpkgs as written in the NixOS wiki FAQ: <https://nixos.wiki/wiki/FAQ/Pinning_Nixpkgs>

```nix
# pinned.nix
import (builtins.fetchTarball {
  # Descriptive name to make the store path easier to identify
  name = "nixpkgs-23.11";
  url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/23.11.tar.gz";
  # Hash obtained using `nix-prefetch-url --unpack <url>`
  sha256 = "1ndiv385w1qyb3b18vw13991fzb9wg4cl21wglk89grsfsnra41k";
}) {}
```

Then we can define our environment with the paths to be included for linking:

```nix
# env.nix
{ pkgs ? import ./pinned.nix }:

pkgs.buildEnv {
  name = "vt2-env";
  paths = [
    pkgs.nodejs_20
  ];
}
```

Once we build this environment and add the result symlink's bin/ directory to our PATH, we will have what we want. From here, we could simply add something like `nix-env` to our .gitignore and do `nix-build env.nix --out-path nix-env` with `PATH_add nix-env` in .envrc with direnv to get what we want.

And that will work for addressing all three of our points above, but we could also go a little further to make this more convenient to use.

## A simple watch_file solution

direnv comes with a watch_file function that you could use to watch for changes to your environment configuration, whether that is `watch_file env.nix`, `watch_dir nix/`, or something more sophisticated. The watch functions let you have the direnv environment reloaded on file changes, which I will take advantage of to update when updating my env file.

Following, I do not want to pay the cost of doing a nix-build each time I evaluate my direnv. It follows then that I need some strategy to prevent nix-builds based on the contents of the environment configuration. In my case, I decided that using a sample of the SHA of the config would suffice in giving me a reasonably unique reference to a given config, which I could then use to build on need and add to my PATH:

```sh
# watch the env.nix for changes to reload direnv
watch_file env.nix

# take the SHA of env.nix
ENV_SHA=$(shasum env.nix | head -c 10)
ENV_PATH="nix-env/$ENV_SHA"

# check if the env has already been built
if [ ! -e $ENV_PATH ]
then
  mkdir -p $(dirname $ENV_PATH)
  nix-build env.nix --out-link $ENV_PATH
fi

# add the specific env bin to our path
PATH_add "$ENV_PATH/bin"
```

This solution could use some improvements in more complex scenarios, but should suffice for many simple uses.

## Conclusion

I hope this has shown a reasonable way that you can provide a development environment using the combination of nixpkgs buildEnv and direnv.

## Notes

### Lorri <https://github.com/nix-community/lorri>

Lorri does something similar to what is written here as a more "full solution", but I was never really happy about how it worked. Also, I don't know what the status of this project is, and do not plan to use this project in the future.

### Cached-nix-shell <https://github.com/xzfc/cached-nix-shell>

This project seems fine but I have not used it much.

### Flakes

I continue to have many issues using flakes. I will have to revisit flakes later.
