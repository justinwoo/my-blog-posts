Many useful GUI apps nowadays are made using Electron, for better and worse. And while consuming these applications should be fairly easy, in reality, they end up with so many dependencies and assumptions about your environment that they can be painful to reproducibly install on different systems. And while we could try to install a cocktail of libraries with our favorite sudo-driven package managers, another nice property we could look towards other than getting rid of sudo calls is to be able to choose exactly what we use as our sources.

In this post, we'll explore how patching the binary in Electron applications with the [PatchELF](https://nixos.org/patchelf.html) utility to replace the dynamic linker and the RPATH, a fundamental of how to prepare programs for reproducible consumption across environments and via NixOS.

## Making a Azure Data Studio derivation

As an example, I will patch up Azure Data Studio so that I can use it. Hopefully none of my readers have to suffer the pain of using MSSQL, but the Azure Data Studio application makes inspecting MSSQL databases not so bad.

The Azure Data Studio folks nicely package releases for consumption on GitHub releases: <https://github.com/Microsoft/azuredatastudio/releases>. With this, we can go grab an appropriate tarball for Linux:

```sh
> nix-prefetch-url https://github.com/Microsoft/azuredatastudio/releases/download/1.3.8/azuredatastudio-linux-1.3.8.tar.gz
path is '/nix/store/1ccybjmyyf9z0nxybq7laq57qnl3qay8-azuredatastudio-linux-1.3.8.tar.gz'
0aq8s6sa4mxbwgqg4j2g720fn07gfyiw14fl6742jlwhssx0zy9s
```

And we can use this for our source. To make life easier for ourselves, we can handle unpacking the tarball ourselves by implementing our own unpack phase:

```nix
# note that we use a recursive set/record to refer to other attributes
pkgs.stdenv.mkDerivation rec {
  # ...

  # yes, i want to run the phases "unpack" and "fixup"
  phases = "unpackPhase fixupPhase";

  targetPath = "$out/azuredatastudio";

  unpackPhase = ''
    mkdir -p ${targetPath}

    # we strip 1 because the tarball comes with one directory:
    # azuredatastudio-linux-x64
    ${pkgs.gnutar}/bin/tar xf $src --strip 1 -C ${targetPath}
  '';
  
  # ...
```

And if we install the derivation in its current state to our environment, it will produce `~/.nix-profile/azuredatastudio`. If we were to symlink the binary now and try to use it though, we might run into some problems like so:

```sh
> azuredatastudio -h
/nix/store/bj468cb9hvadyxmn1kwznyk5wwgj4fw1-azuredatastudio/azuredatastudio/bin/../azuredatastudio: error while loading shared libraries: libnode.so: cannot open shared object file: No such file or directory
```

...which is about what we normally get if we try to `sudo apt-get install [something]` without having a cocktail of libraries in our environment.

### Preparing our RPATH

Currently, the convention for preparing RPATH for Electron apps is to use the Atom environment libPath and then add any more that need to be added. For some apps, just `atomEnv.libPath` will be enough. In our case, we need to provide some more paths for Azure Data Studio:

```nix
  # with pkgs; allows us to refer to attributes of pkgs without the prefix
  # atomEnv.libPath below refers to pkgs.atomEnv.libPath
  rpath = with pkgs; lib.concatStringsSep ":" [
    atomEnv.libPath
    targetPath
    "${targetPath}/resources/app/extensions/mssql/sqltoolsservice/Linux/1.5.0-alpha.60"
  ];
```

And this will give us all of the libraries we actually need to run Azure Data Studio.

### Patching the Azure Data Studio binary

With the RPATH ready, we can now patch the binary in the unpacked output. To do this, we can use the PatchELF utility with the dynamic linker available from nixpkgs:

```nix
  fixupPhase = ''
    # patch the azuredatastudio binary
    patchelf \
      --set-interpreter "${dynamic-linker}" \
      --set-rpath "${rpath}" \
      ${targetPath}/azuredatastudio
    
    mkdir -p $out/bin
    # finally, we reproduce the output as a symlink of the patched binary
    ln -s ${targetPath}/bin/azuredatastudio $out/bin/azuredatastudio
  '';
```

Now this will have everything it needs.

For reference, this is the full derivation:

```nix
{ pkgs ? import <nixpkgs> {} }:

let
  dynamic-linker = pkgs.stdenv.cc.bintools.dynamicLinker;

in pkgs.stdenv.mkDerivation rec {
  name = "azuredatastudio";
  src = pkgs.fetchurl {
    url = "https://github.com/Microsoft/azuredatastudio/releases/download/1.3.8/azuredatastudio-linux-1.3.8.tar.gz";
    sha256 = "0aq8s6sa4mxbwgqg4j2g720fn07gfyiw14fl6742jlwhssx0zy9s";
  };

  phases = "unpackPhase fixupPhase";

  targetPath = "$out/azuredatastudio";

  unpackPhase = ''
    mkdir -p ${targetPath}
    ${pkgs.gnutar}/bin/tar xf $src --strip 1 -C ${targetPath}
  '';

  rpath = with pkgs; lib.concatStringsSep ":" [
    atomEnv.libPath
    targetPath
    "${targetPath}/resources/app/extensions/mssql/sqltoolsservice/Linux/1.5.0-alpha.60"
  ];

  fixupPhase = ''
    patchelf \
      --set-interpreter "${dynamic-linker}" \
      --set-rpath "${rpath}" \
      ${targetPath}/azuredatastudio
    mkdir -p $out/bin
    ln -s ${targetPath}/bin/azuredatastudio $out/bin/azuredatastudio
  '';
}
```

## Installing and running our derivation

If we want to just install Azure Data Studio to our profile, we can source the file to install from nix-env: `nix-env -f [our-derivation.nix] -i`. In my case, I have named the source file `default.nix`, so I can install this with `nix-env -f default.nix -i`. I can now use Azure Data Studio and see that it is in my profile and works:

```sh
> which azuredatastudio
/home/justin/.nix-profile/bin/azuredatastudio

> azuredatastudio --version
1.3.8
84009f65ec6297d1cc16b1b2ec8f6fba9f5be2a1
x64
```

## Conclusion

Hopefully this has shown you how patching binaries works to make them reproducible across environments, not only for electron apps, but for many other programs. Once you know how to use PatchELF, you'll be able to reliably package up various programs for your own consumption without the need to go through various expensive build processes, consuming them from any release tarball.

In the future, I'll also talk about how wrapProgram can be a feasible alternative for programs and scripts that need to be run with various environment conditions.

## Links

* PatchELF: <https://nixos.org/patchelf.html>
* Azure Data Studio Nix: <https://github.com/justinwoo/azuredatastudio-nix>
* Example: VSCode derivation from NixPkgs: <https://github.com/NixOS/nixpkgs/blob/c8c53fcb1154999dabac350bebe12611a0f75024/pkgs/applications/editors/vscode/default.nix>
