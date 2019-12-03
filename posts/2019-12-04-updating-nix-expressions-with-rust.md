# Updating Nix expressions with Rust

Earlier this year, the nixpkgs-fmt project was announced, a project making use of the rnix parser library which implements a parser for Nix in Rust. As its name suggests, nixpkgs-fmt does reformatting of Nix files mostly to the nixpkgs style guide.

Around the same time, I wrote a tool to update Nix files using the node tree-sitter bindings with tree-sitter-nix in PureScript. However, I soon ran into problems with trying to use the node tree-sitter bindings due to the way its releases work, requiring a strict combination of platform, node version, node-gyp, and various library dependencies.

Rather than to try to tackle these problems, I opted instead to try to make use of the nixpkgs-fmt code, especially since I was mostly formatting my Nix files with nixpkgs-fmt by the time these issues came up.

In this article, I will go through some of the parts I used out of nixpkgs-fmt along with rayon to prepare data for updates to Nix expressions in parallel.

## Initial setup

Other than the usual `cargo init`, I needed to fork the nixpkgs-fmt repo to actually expose modules and their contents as public, rather than crate-local unexported. I won't bore you with the details, but it mostly involved changing `pub(crate)` to `pub`.

```
[dependencies]
nixpkgs-fmt = { git = "https://github.com/justinwoo/nixpkgs-fmt", tag = "freedom-2019-11-26" }
rnix = "0.7.1"
rayon = "1.2.1"
serde = { version = "1.0.103", features = ["derive"] }
serde_json = "1.0.42"
```

The serde dependency is only used to parse some JSON output from nix-prefetch-git, as we will see later.

## Main(1)

While the end product will have a different entry point, we can instead just look at what it means to handle a single file.

Mainly, what we will need to do are:

* Read in a file into some type that `rnix` will parse into an AST
* Take the generated AST and grab its root node
* Use the root node to go through and apply some updates
* Take the result of the updates and give it to nixpkgs-fmt to reformat

The result looks like so:

```rust
fn main1(file: &Path) -> io::Result<()> {
    let input = fs::read_to_string(file)?;

    let ast = rnix::parse(&input);
    let root_node = ast.node();
    let result = update_fetch::format(&root_node).to_string();

    let output = nixpkgs_fmt::reformat_string(&result);
    if input != output {
        fs::write(file, &output)?;
    }
    Ok(())
}
```

## Format

Our format function gets a big long, so I will first collapse its sections and go through its parts.

```rust
use nixpkgs_fmt::{
    engine::fmt_model::FmtModel, tree_utils::walk_non_whitespace, AtomEdit, FmtDiff,
};
use rayon::prelude::*;
use rnix::{SyntaxNode, TextRange};

pub struct Replacement {
    pub delete: TextRange,
    pub method: core::ReplacementMethod,
}
```

In nixpkgs-fmt, there is a FmtModel that I deal with to prepare edits to be made to Nix expressions. These take `AtomEdit`s, which require that I provide a `TextRange` to be deleted, and a replacement String-like type for the contents to replace these.

There is one problem though: I can't pass around the syntax node references to rayon for parallel processing. That's not too much of a problem though, as I don't really need the node but only some of the information from it to process. For this, I made the type `Replacement` which contains this text range and information on how the range is to be updated.

So the solution requires a few steps, only one of which needs to be handled in parallel:

* Prepare a new `FmtDiff`
* Prepare all of the replacements that need to be done by walking the root node
* Process the replacements in parallel to get all of the edits that need to be performed
* Get these edits and shove them into the `FmtModel` one by one
* Produce some data type that will later be printed

```rust
/// The main entry point for formatting
pub fn format(root: &SyntaxNode) -> FmtDiff {
    let mut model = FmtModel::new(root.clone());

    let replacements: Vec<Replacement> = walk_non_whitespace(root)
        .flat_map(|element| { /* some element handling */ })
        .collect();

    let edits: Vec<AtomEdit> = replacements
        .par_iter() // iterate in parallel using rayon
        .flat_map(|r| {
          /* some handling of edits */
        })
        .collect();

    for edit in edits {
        model.raw_edit(edit);
    }

    model.into_diff()
}
```

Then let's look at the replacement reproduction and edit production separately.

## Replacements

The actual updating I want to do with this program is to update fetch expressions, which are most commonly one of four things: `fetchTarball`, `fetchurl`, `fetchFromGitHub`, and `fetchgit`. These functions are mostly called with attribute sets, so we can take this approach:

* Check if the current node is a attribute set
* Build up its keys and values into a hashmap to work with
* Check if the previous node looks like a fetch function, implying that the current attribute set node is an argument to this function
* Prepare a `Replacement`, specifying the replacement method with relevant information and the text range

From the above format function, this looks like so:

```rust
    let replacements: Vec<Replacement> = walk_non_whitespace(root)
        .flat_map(|element| {
            element
                .as_node()
                .and_then(|x| node::get_node_attr_set(&x))
                .and_then(|node: &SyntaxNode| {
                    let set = node::attr_set_binds_to_hashmap(&node);
                    core::handle_fetch(&node, &set).map(|method| {
                        let range = node.text_range();
                        let delete = TextRange::offset_len(range.start(), range.len());
                        Replacement { delete, method }
                    })
                })
        })
        .collect();
```

We need to call `.as_node` to make sure we are working with an actual node here (instead of just a token), then we string together calls to check if the node is an attribute set and make a hashmap and create a `Replacement` as necessary.

The fetch handling is then implemented as attempting to get any of the alternatives of the matches:

```rust
pub enum ReplacementMethod {
    FetchTarball { url: String },
    FetchUrl { url: String },
    FetchFromGitHub { owner: String, repo: String },
    Fetchgit { url: String },
}

pub fn handle_fetch(node: &SyntaxNode, attrs: &AttrHashMap) -> Option<ReplacementMethod> {
    handle_fetch_tarball(node, attrs)
        .or_else(|| handle_fetch_url(node, attrs))
        .or_else(|| handle_fetch_github(node, attrs))
        .or_else(|| handle_fetchgit(node, attrs))
}
```

Let's just look at the definition of `handle_fetchgit` to get an idea of how this works.

```rust
fn handle_fetchgit(node: &SyntaxNode, attrs: &AttrHashMap) -> Option<ReplacementMethod> {
    let prev_node_string: String = node.prev_sibling()?.text().to_string();
    let prev_contains_fetchgit = prev_node_string.contains("fetchgit");

    if !prev_contains_fetchgit {
        return None;
    }

    let url = attrs.get("url")?;
    let _rev = attrs.get("rev")?;
    let _sha256 = attrs.get("sha256")?;

    Some(ReplacementMethod::Fetchgit {
        url: url.to_owned(),
    })
}
```

First, we look at the previous node as talked about above:

> Check if the previous node looks like a fetch function, implying that the current attribute set node is an argument to this function

Then we extract out the url to be used, and merely check the existence of the `rev` and `sha256` attributes to make sure this had some required fields beforehand. This is then put into a `Replacement` under the `Fetchgit` constructor.

This is about all that is required for us to prepare our replacements.

## Edits

Applying the edits then becomes more routine:

```rust
    let edits: Vec<AtomEdit> = replacements
        .par_iter()
        .flat_map(|r| {
            core::prepare_replacement(&r.method).and_then(|string| {
                Some(AtomEdit {
                    delete: r.delete,
                    insert: string.into(),
                })
            })
        })
        .collect();
```

`prepare_replacement` here will use the `ReplacementMethod` data from before to prepare the appropriate replacements:

```rust
pub fn prepare_pub type ReplacementString = String;

replacement(method: &ReplacementMethod) -> Option<ReplacementString> {
    match method {
        ReplacementMethod::FetchTarball { url } => prepare_tarball_replacement(url),
        ReplacementMethod::FetchUrl { url } => prepare_url_replacement(url),
        ReplacementMethod::FetchFromGitHub { owner, repo } => {
            prepare_github_replacement(owner, repo)
        }
        ReplacementMethod::Fetchgit { url } => prepare_git_replacement(url),
    }
}
```

Again, let's look at the fetchgit case:

```rust
#[derive(Serialize, Deserialize, Debug)]
struct PrefetchGit {
    rev: String,
    sha256: String,
}

fn prepare_git_replacement(url: &String) -> Option<ReplacementString> {
    let prefetch_attempt = process::Command::new("nix-prefetch-git")
        .arg(format!("{}", remove_quotes(url)))
        .output()
        .expect("Error: Failed to launch nix-prefetch-git.");

    if prefetch_attempt.status.success() {
        let json = String::from_utf8(prefetch_attempt.stdout).unwrap();
        let prefetch: PrefetchGit = serde_json::from_str(&json).unwrap();

        println!("Fetched rev and sha256 for {}", url);
        Some(format!(
            r#"{{
                url = {};
                rev = "{}";
                sha256 = "{}";
            }}"#,
            url, prefetch.rev, prefetch.sha256,
        ))
    } else {
        println!("Failed to prefetch git repo: {}", url);
        None
    }
}
```

We use `process::Command` in order to fire off `nix-prefetch-git`, and use serde in order to decode he response to build a replacement string.

And this is about it, where we have now prepared the replacement strings to be used for the format model.

## Main (top level)

Finally, we can go up to the very top level of our program, where we handle the arguments given to our program.

```rust
fn main() {
    let mut args: Vec<String> = env::args().collect();

    args.remove(0);

    if args.is_empty() {
        println!("{}", EXPECT_FILE_PATH_ARG_MSG.trim());
        process::exit(1);
    }

    args.par_iter()
        .map(|x| {
            let path = Path::new(x);
            let result = main1(path);
            match result {
                Ok(_) => println!("Finished {}", x),
                Err(e) => {
                    println!("Error on {}: {}", x, e);
                }
            }
        })
        .collect::<()>();
}
```

And so, we can handle each file in parallel also.

## Conclusion

Hopefully this has shown that rewriting Nix code with Rust can be done fairly smoothly, by using the existing work done in rnix and nixpkgs-fmt.

## Links

* update-fetch: <https://github.com/justinwoo/update-fetch>

* nixpkgs-fmt: <https://github.com/nix-community/nixpkgs-fmt>

* rnix-parser: <https://github.com/nix-community/rnix-parser>
