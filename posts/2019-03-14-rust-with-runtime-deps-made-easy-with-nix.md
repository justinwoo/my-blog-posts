# Rust with Runtime Deps made easy with Nix

Recently, I have written some very critical things about Nix on Twitter, but I'm still able to use it to get things done that I normally otherwise would need to provide many workarounds for. In this article, I will write about how I can easily build a Rust project with runtime dependencies being readily available with Nix, which is typically not such an easy task.

## The problem

For a while, I've wanted to rewrite my ytcasts program, which searches a youtube page for links to download audio from. The original program was written in PureScript and used an HTML parsing library and all, but I wanted to instead work with some simple command line utilities. And while I could use Perl, I decided to instead reach for a typed language that would make working with processes fairly easy. So I ended up using Rust in the end.

## `main.rs`

There's not very much involved in this program. It expects one argument for a channel to scrape, and then ensures a SQLite database of download entries to keep track of previously downloaded videos. It then uses a series of `curl`, html normalizing/selecting utils, and `xsltproc` to perform XSLT transformations to parse information I need out.

```rust
use std::env;
use std::process::Command;

struct Target {
    title: String, // hello world
    href: String,  // /watch?v=myCode
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let page_url = args.get(1).expect(EXPECT_YOUTUBE_PATH_URL_MESSAGE);

    ensure_sqlite_db();

    let pick_xsl = env::var_os("PICK_XSL")
        .map(|x| x.into_string().unwrap())
        .unwrap_or_else(|| "pick.xsl".to_string());

    let targets = get_targets(page_url, &pick_xsl);

    for target in targets {
        download_target(target);
    }

    println!("done");
}
```

The commands run in the shell from my program all call this function:

```rust
fn run_command(command: &str) -> String {
    let command_ref = &command;
    let attempt = Command::new("bash")
        .arg("-c")
        .arg(command_ref)
        .output()
        .expect("Failed to launch bash command");

    if attempt.status.success() {
        let result: String = String::from_utf8(attempt.stdout)
            .unwrap_or_else(|_| panic!("Invalid output from command {}", command));
        result.trim().to_string()
    } else {
        panic!("Command failed: {}", command)
    }
}
```

So with commands being run in bash calls, these kinds of commands are run and processed:

```rust
fn get_targets(page_url: &str, pick_xsl: &str) -> Vec<Target> {
    let output = run_command(&format!(
        r#"
      content=$(curl {}| hxnormalize -x | hxselect '.yt-uix-tile-link') \
      && xml="<results>$content</results>" \
      && echo $xml | xsltproc {} -
    "#,
        page_url, pick_xsl
    ));
```

Notice that the `pick_xsl` file is an argument that was passed from main, which may optionally be an environment variable value or just the relative `pick.xsl` file. We'll see later how I never have to define this environment variable in my profile.

I think you get the gist now of what this program is like. If you really would like to read the whole thing (roughly 125 lines), you can read it here: <https://github.com/justinwoo/ytc2/blob/master/src/main.rs>

## The Rust build

Building a Rust package on nixpkgs (as of March 2019) is fairly simple if you're willing to let Cargo do the driving, which may or may not work for you, depending on the level of guarantees you want. With Cargo, we only need to specify some normal things like name, version, where the sources live (which is the current directory in this case), and the sha for the Cargo build:

```rust
{ pkgs ? import <nixpkgs> {} }:

let
  binary = pkgs.rustPlatform.buildRustPackage rec {
    name = "ytc2-rs";
    version = "0.1.0";
    src = ./.;
    cargoSha256 = "0jacm96l1gw9nxwavqi1x4669cg6lzy9hr18zjpwlcyb3qkw9z7f";
  };
  
  # ...
```

And that's all we need. To get the SHA, you might even just run this with an empty string for the value, and use the result of the build.

## The runtime requirements

So you should be asking, other than the environment variable `PICK_XSL` from earlier, what all other things are involved? I wrote up a list of these that pull in the derivations I need from nixpkgs:

```nix
{ pkgs ? import <nixpkgs> {} }:

{
  inherit (pkgs)
  curl
  html-xml-utils
  libxslt
  jq
  sqlite
  youtube-dl;
}
```

With this list of requirements, we can get to putting together the whole derivation.

## Our derivation

To provide all of these requirements and also the `PICK_XSL` environment variable, we can make use of the `makeWrapper` shell function: <https://nixos.org/nixpkgs/manual/#ssec-stdenv-functions>

In short, this allows us to wrap some binary and set or prefix some environment variables. In our case, we need to provide for `curl`, `jq,` `sqlite`, and all of the other things to be available on the `PATH` and as the first results on lookup, and we need the `PICK_XSL` variable to be set to the location of the `pick.xsl` file. To do so, we simply add `makeWrapper` from nixpkgs to our build inputs.

Our whole derivation comes out like so:

```nix
{ pkgs ? import <nixpkgs> {} }:

let
  binary = pkgs.rustPlatform.buildRustPackage rec {
    name = "ytc2-rs";
    version = "0.1.0";
    src = ./.;
    cargoSha256 = "0jacm96l1gw9nxwavqi1x4669cg6lzy9hr18zjpwlcyb3qkw9z7f";
  };

  # get our requirements from before
  requirements = import ./requirements.nix { inherit pkgs; };

# build a derivation by running a command
in pkgs.runCommand "ytc2" {

  # provide some attributes
  name = "ytc2";
  buildInputs = [
    pkgs.makeWrapper
  ];
} ''
    # a derivation needs to produce some $out files
    mkdir -p $out/bin

    # first, install our Cargo-built binary to $out/bin
    install -D -m555 -t $out/bin ${binary}/bin/ytc2

    # then wrap our program with the appropriate prefixed variables
    wrapProgram $out/bin/ytc2 \
      --prefix PICK_XSL : ${binary.src}/pick.xsl \
      --prefix PATH : ${pkgs.lib.makeBinPath (builtins.attrValues requirements)}
  ''
```

And that's it. We can run `nix-build` for good measure and inspect the result in `./result`:

```bash
> fd . result/
result/bin
result/bin/ytc2
```

And for sadistic pleasure, we can look at the result of wrapProgram:

```bash
> cat result/bin/ytc2
#! /nix/store/cinw572b38aln37glr0zb8lxwrgaffl4-bash-4.4-p23/bin/bash -e
export PICK_XSL='/nix/store/y2bgfn52d4z5zahis6870q2q6dac4pdi-ytc2/pick.xsl'${PICK_XSL:+':'}$PICK_XSL
export PATH='/nix/store/1w5kx3j1ics34pgq8cb19n15ffmdzj8p-curl-7.64.0-bin/bin:/nix/store/k92xpgznnmhzdb16arwgh41igi0j92ms-html-xml-utils-7.7/bin:/nix/store/7fj1faysp1lrfiwp5af5gdqj4kqgcs29-jq-1.6-bin/bin:/nix/store/ss50s80lf1bwhxcmjjl3dgycmgpqp7yp-libxslt-1.1.33-bin/bin:/nix/store/lma5my05iidygwc00dlk1g8s40b4vd64-sqlite-3.26.0-bin/bin:/nix/store/bcx9h1nq35iaqg4agsvldryqgcrmrgva-python3.7-youtube-dl-2019.03.01/bin'${PATH:+':'}$PATH
exec -a "$0" "/nix/store/xmshmqv48ci5kbxlr45g2015klpc149l-ytc2/bin/.ytc2-wrapped"  "${extraFlagsArray[@]}" "$@"
```

And as you can see, this what Nix actually does best. It takes derivations and builds them into the outputs, then makes specific outputs of derivations available for other derivations to consume. By doing this, I'm able to pin down runtime requirements to specific outputs of derivations, so that I don't have to deal with trying to install prerequisites manually or have to deal with some unholy incantation of `configure; make; make install`.

This is the actual thing that Nix does well and is important, that dynamic requirements can instead be turned into static requirements. I do not have to try to wait for someone to make some "static executable" that might not even work, instead I am put in the driver's seat of how I want to provide the required inputs.

## Conclusion

Hopefully this has shown you some of the things Nix can make easy for you to do. In the end, Nix is like any other tool, with some investment in learning required to either use or hate. If you haven't tried to use Nix much before, I encourage you to give it some tries to learn something from the experience. This post might give you some help in getting started: <https://qiita.com/kimagure/items/8b4df59236717e54a2bc>.

## Links

* This project: <https://github.com/justinwoo/ytc2>
* Putting your own derivations in Nix Profile: <https://qiita.com/kimagure/items/8b4df59236717e54a2bc> or <https://github.com/justinwoo/my-blog-posts/blob/master/posts/2019-01-12-putting-your-own-derivations-in-nix-profile.md>
