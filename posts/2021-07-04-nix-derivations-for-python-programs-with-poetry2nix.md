# Nix derivations for Python programs with Poetry2Nix

For a long time, I haven't been able to find any solutions for creating Nix derivations of Python programs. Problems such as...

* not being able to survive different versions of Nixpkgs (updating pinned Nixpkgs would destroy my derivation)
* not being able to get versions solved either by pip or some tool that tries to use pip requirements
* not being able to automatically get some derivation, instead having to specify some Python environment completely with hand-written overrides of packages and their versions (with SHAs for the upstream sources)

I had originally run into some various problems with trying to get this to work in 2019, but gave up then.

Thankfully, some brave people made a solution that works around Poetry, a package manager for Python that actually generates a lockfile for versions of a project that have been solved for.

## Motivation: wrapping `awsebcli`

Anyone who has tried to use the `awsebcli` has probably run into some various problems with their Python/pip environment and the dependencies in this package when also working with other packages, namely, `awscli`, which also has its own set of `botocore` dependencies and whatever.

## Using Poetry

With Poetry, I can at least create an environment in which awsebcli will work. To do this, I ran a nix-shell with poetry to get everything set up:

```bash
test-awsebcli $ nix-shell -p poetry

# we make our poetry project at first with their prompts
test-awsebcli $ poetry init
[...poetry prompt things...]
Search for package to add (or leave blank to continue): awsebcli
[...poetry prompt things...]

# then have poetry solve everything and generate the lock file
test-awsebcli $ poetry lock
[..poetry things...]
Writing lock file

test-awsebcli $ ls
poetry.lock  pyproject.toml

test-awsebcli $ poetry shell
Spawning shell within /home/justin/.cache/pypoetry/virtualenvs/test-awsebcli-VH3cZtax-py3.8
. /home/justin/.cache/pypoetry/virtualenvs/test-awsebcli-VH3cZtax-py3.8/bin/activate

test-awsebcli $ which eb
/home/justin/.cache/pypoetry/virtualenvs/test-awsebcli-VH3cZtax-py3.8/bin/eb
test-awsebcli $ eb --version
EB CLI 3.20.0 (Python 3.8.9)
```

However, we are in for some fun when we actually package this up, due to how dependencies are specified in the setup.py of awsebcli.

## Using Poetry2Nix

Using Poetry2Nix is supposed to be very easy, and it would be, if it weren't for Python and its users.

```nix
{ pkgs ? import <nixpkgs> { } }:

let
  poetry2nix = pkgs.callPackage
    (pkgs.fetchFromGitHub {
      owner = "nix-community";
      repo = "poetry2nix";
      rev = "e3c3e14e99ca3d573ef84644754a0daa97803e3e";
      sha256 = "01rs9lxw1s8qy7g9m0dchjf5k06sjcd5bbxzi71p2n08xgsdrwlr";
    })
    { };
  env = poetry2nix.mkPoetryEnv {
    projectDir = ./.;
  };
in
env
```

If you're lucky, this is all you will have to do. You bring in Poetry2Nix into your project and use its `mkPoetryEnv` to make an environment where then you can use specific program you want from it.

However, we can find some problems when we run this:

```bash
test-awsebcli $ nix-build

ERROR: Could not find a version that satisfies the requirement blessed>=1.9.5 (from awsebcli==3.20.0) (from versions: none)
ERROR: No matching distribution found for blessed>=1.9.5 (from awsebcli==3.20.0)
```

So fine, we need to manually specify that we need `blessed`. Shouldn't be too bad either with overrides as documented in Poetry2Nix.

```nix
  env = poetry2nix.mkPoetryEnv {
    projectDir = ./.;
    overrides = poetry2nix.overrides.withDefaults
      (self: super:
        {
          awsebcli = super.awsebcli.overrideAttrs (old: {
            buildInputs = old.buildInputs ++ [
              self.blessed
            ];
          });
        }
      );
  };
```

```bash
test-awsebcli $ nix-build

ERROR: Could not find a version that satisfies the requirement docker-compose<1.26.0,>=1.25.2 (from awsebcli==3.20.0) (from versions: none)
ERROR: No matching distribution found for docker-compose<1.26.0,>=1.25.2 (from awsebcli==3.20.0)
```

In the end, I found out that `docker-compose` is only conditionally installed and not in our lockfile as a result, so it is not in our Poetry2Nix Python environment. In fact, this version range given is not even for the latest version of `docker-compose`. So in the end, I had to edit `pyproject.toml` manually to add the dependency:

```toml
[tool.poetry.dependencies]
python = "^3.8"
awsebcli = "^3.20.0"
docker-compose = "1.25.2"
```

Then from here, we run `poetry lock` again and then add this to the overrides.

```nix
          awsebcli = super.awsebcli.overrideAttrs (old: {
            buildInputs = old.buildInputs ++ [
              self.blessed
              self.docker-compose
            ];
          });
```

Now this will actually work as expected.

```bash
test-awsebcli $ nix-build
/nix/store/blahblah-python3-3.8.9-env

test-awsebcli $ ./result/bin/eb --version
EB CLI 3.20.0 (Python 3.8.9)
```

## Conclusion

So we can see that Poetry2Nix is quite nice and lets us quite easily modify our environment so that we can provide all the dependencies we need that are so commonly not actually specified in the `requires` blocks in `setup.py` of Python packages.

## Links

* poetry2nix https://github.com/nix-community/poetry2nix
* this example https://github.com/justinwoo/test-awsebcli
