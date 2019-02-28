---
title: Making a simple Haskell CLI without anything fancy
tags: haskell
author: kimagure
slide: false
---

Nowadays if you read about Haskell on Twitter, you will quickly find that everyone is constantly screaming about some "advanced" techniques and trying to flex on each other. In reality, all you need to write Haskell is a Haskell compiler. In this post, I'll show you how I ported a Go program I wrote using nothing but GHC and its accompanying boot libraries. I won't even use Cabal or Stack, because I don't need any more libraries to write simple CLIs.

## What are these boot libraries?

These are the many libraries that GHC comes with: <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/8.6.1-notes.html#included-libraries>

For my purposes, I will be using only `base`, `text`, and `process`, but you may find other things you like here.

## How do you avoid Cabal and Stack?

There's roughly three main parts involved:

* Install or have GHC available in some way. I used a glorified `nix-env -i ghc` from a file I keep version controlled, but you might use some other method.

* Don't install Cabal (if you don't have it already).

* Don't install Stack (if you don't have it already).

Then I create a Haskell source file. In this case I used `touch prefetch-github.hs`.

To run my program, I can later run `runhaskell prefetch-github.hs`. To prepare a binary for me to use later, I can run `ghc prefetch-github.hs -o prefetch-github`. More on this later when we run our program and package it up in some kind of way.

## The rewrite from Go to Haskell

Why did I rewrite from Go? The reasons are fairly simple:

* I don't really use Go
* I didn't want to install the editor tooling for Go again
* Haskell is fun when you write whatever you want, not what Twitter has pressured you into writing

The third point is admittedly quite hard to get over sometimes, but it's key to having fun writing Haskell.

## Prefetch-Github

This is a program I originally wrote in Go, which calls nix-prefetch-git with some various different arguments. It then parses the output of nix-prefetch-git to prepare to what I want to get out. For the most part, I want to get a Nix set (record) expression to feed into nixpkgs.fetchFromGitHub or nixpkgs.fetchgit.

So this boils down to some various sub goals that need to be solved:

* Some strings and some templating functions
* Parse arguments
* Create a process to run nix-prefetch-git
* Parse the output

### Strings and templating

Haskell is an old language lacking some typical conveniences, such as literal multiline strings. While many people want to really jump straight into looking for various Template Haskell solutions for having multiline strings, I decided to go for the simplest solution: newline escaping.

This really involves nothing more than what you might imagine:

```hs
helpMessage :: String
helpMessage = "Usage of prefetch-github:\n\
\  -branch\n\
\    Treat the rev as a branch, where the commit reference should be used.\n\
\  -fetchgit\n\
\    Print the output in the fetchGit format. Default: fromFromGitHub\n\
\  -hash-only\n\
\    Print only the hash.\n\
\  -owner string\n\
\    The owner of the repository. e.g. justinwoo\n\
\  -repo string\n\
\    The repository name. e.g. easy-purescript-nix\n\
\  -rev string\n\
\    Optionally specify which revision should be fetched."
```

Is it kind of ugly? I guess. I guess it also does technically give me a bit more control over the layout.

If I consider the fetchFromGitHub output I need to output later, the same can be applied with just some strings being appended.

```hs
-- | Required arg for Owner of a repo (user or org)
newtype Owner = Owner String

-- | Required arg for repo (e.g. 'readme' in justinwoo/readme)
newtype Repo = Repo String

-- | Revision (git)
newtype Rev = Rev String

newtype Sha = Sha String

mkGithubTemplate :: Owner -> Repo -> Rev -> Sha -> String
mkGithubTemplate (Owner owner) (Repo repo) (Rev rev) (Sha sha) = "{\n\
\  owner = \"" <> owner <> "\";\n\
\  repo = \"" <> repo <> "\";\n\
\  rev = \"" <> rev <> "\";\n\
\  sha256 = \"" <> sha <> "\";\n\
\}"
```

### Parsing arguments

According to Twitter, you should use [optparse-applicative](https://github.com/pcapriotti/optparse-applicative), and I kind of agree. It's a nice library and can help you structure what you want. But I also believe that pattern matching on lists of strings is fun, and even more so if you do it recursively. This is why I chose to just use System.Environment.getArgs and pattern match away:

```hs
main :: IO ()
main = do
  args <- Env.getArgs
  case args of
    [] -> help
    ["help"] -> help
    ["-help"] -> help
    _ -> main' args

main' :: [String] -> IO ()
main' args = do
  owner <- parseOwner args
  repo <- parseRepo args
  -- ...
  
help :: IO ()
help = putStrLn helpMessage
```

For parsing `Owner` and `Repo`, I employ the power of recursion with pattern matching:

```hs
parseOwner :: [String] -> IO Owner
parseOwner ("-owner" : owner : _) = pure $ Owner owner
parseOwner (_ : xs) = parseOwner xs
parseOwner [] = fail "owner must be specified in args. see help."

parseRepo :: [String] -> IO Repo
parseRepo ("-repo" : repo : _) = pure $ Repo repo
parseRepo (_ : xs) = parseRepo xs
parseRepo [] = fail "repo must be specified in args. see help."

parseRev :: [String] -> Maybe Rev
parseRev ("-rev" : rev : _) = Just $ Rev rev
parseRev (_ : xs) = parseRev xs
parseRev [] = Nothing
```

### Creating a process to run `nix-prefetch-git`

Like before, I can template the command I want to run:

```hs
mkNixPrefetchGitCmd :: Owner -> Repo -> String -> String
mkNixPrefetchGitCmd (Owner owner) (Repo repo) revArg = cmd
  where
    url = "https://github.com/" <> owner <> "/" <> repo <> ".git/"
    cmd = "GIT_TERMINAL_PROMPT=0 nix-prefetch-git " <> url <> " --quiet --rev " <> revArg
```

Then I can put some functions from System.Process to work to run this:

```hs
  let cmd = mkNixPrefetchGitCmd owner repo revArg
  let cp = Proc.shell cmd
  out <- Proc.readCreateProcess cp ""
```

And this will get the result back from the shell for me, while failing the program if the process returns a non-zero exit code.

### Parsing the output

The output of nix-prefetch-git is some JSON. While many will tell you that Aeson is "the" solution for deseserializing JSON, there's a few things to consider:

* The valid output is always a record
* This output will always have "url", "rev", and "sha" fields.
* These fields will always be placed on new lines.
* The value is always the second element when splitting a line by spaces, of which the value can be obtained by stripping quotes and commas.

With this knowledge, we find that we don't need to even use Aeson this time around.

```hs
-- | Parse out the result of running nix-prefetch-git
--   "url": "https://github.com/justinwoo/easy-purescript-nix",
--   "rev": "54266e45aeaebc78dd51a40da36e9840a8a300dd",
--   "date": "2019-02-08T01:59:41+02:00",
--   "sha256": "1swjii6975cpys49w5rgnhw5x6ms2cc9fs8ijjpk04pz3zp2vpzn",
--   "fetchSubmodules": false
parseNixPrefetchGitResult :: String -> IO (Url, Rev, Sha)
parseNixPrefetchGitResult out = do
  case handleResult <$> mUrl <*> mRev <*> mSha of
    Just x -> x
    Nothing -> fail $ "failed to parse nix-prefetch-git output: " <> out
  where
    texts = Text.lines $ Text.pack out
    takeProp key
        = Text.filter (\c -> c /= '"' && c /= ',')
        . (\xs -> xs !! 1)
        . Text.words
      <$> List.find (Text.isInfixOf . Text.pack $ "\"" <> key <> "\"") texts
    mUrl = takeProp "url"
    mRev = takeProp "rev"
    mSha = takeProp "sha256"
    mkString ctr txt = ctr $ Text.unpack txt
    handleResult url rev sha =
      if Text.pack failedPrefetchRev `Text.isInfixOf` rev
        then fail $ "nix-prefetch-url could not find the repo:\n" <> out
        else pure $ (mkString Url url, mkString Rev rev, mkString Sha sha)
```

### Putting it all together

With these parts put together, I have the whole regular mode of operation:

```hs
main' :: [String] -> IO ()
main' args = do
  owner <- parseOwner args
  repo <- parseRepo args

  let cmd = mkNixPrefetchGitCmd owner repo revArg
  let cp = Proc.shell cmd
  out <- Proc.readCreateProcess cp ""

  (url, rev, sha@(Sha sha')) <- parseNixPrefetchGitResult out

  case (hashOnly, fetchGit) of
    (True, _) -> putStrLn sha'
    (_, False) -> putStrLn $ mkGithubTemplate owner repo rev sha
    (_, True) -> putStrLn $ mkGitTemplate url rev sha

  where
    fetchGit = parseFetchGit args
    asBranch = parseAsBranch args
    hashOnly = parseHashOnly args
    revArg = case (parseRev args, asBranch) of
      (Just (Rev r), True) -> refsPrefix <> r
      (Just (Rev r), False) -> r
      (Nothing, _) -> ""
```

And this is really mostly it. This program can now be run either with `runghc prefetch-github.hs [args]` or compiled to a binary by running `ghc prefetch-github.hs -o prefetch-github`:

```
> runhaskell prefetch-github.hs -owner justinwoo -repo prefetch-github
{
  owner = "justinwoo";
  repo = "prefetch-github";
  rev = "ecc358529592f403d24a955c293922124c4354f7";
  sha256 = "1wcyzmbrs0rzva7jwnqa4vqr34z1sv1cigpyyiaajkf8bx29pamw";
}
```

You can see the project here: <https://github.com/justinwoo/prefetch-github>

## Conclusion

Hopefully this has shown you that you can write Haskell without either having to use Cabal/Stack or having to do all kinds of random things Twitter tells you to do. In reality, being familiar with writing "boring" code like will help you the most not only in understanding more "advanced" topics, but being employable working with any programming language, be it Haskell, PureScript, JavaScript, or whatever else.
