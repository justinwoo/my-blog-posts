# Patching binaries in Nix is easy and fun

Long time readers of my posts probably know that I do not like to build things from source for various reasons, including but not limited to:

- downloading toolchains
- compilation times and costs
- source duplication in nix store
- source duplication of local file trees in nix store
- usage of binary stores
- usage of proprietary binary stores

So the answer ends up being to use the binaries that a given project will emit. The problem? You must prepare the environment around the binary that you have downloaded to be able to run the binary.

## Typical `ldd` usage based on globally installed libraries

Printing the shared object dependencies of your binary, we can see what all is used by whatever binary we have.

```shell
$ ldd zephyr/zephyr
zephyr/zephyr: /lib/x86_64-linux-gnu/libm.so.6: version `GLIBC_2.29' not found (required by zephyr/zephyr)
        linux-vdso.so.1 (0x00007ffe2f98c000)
        libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007fd8be274000)
        libtinfo.so.6 => not found
        libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007fd8be055000)
        libstdc++.so.6 => /usr/lib/x86_64-linux-gnu/libstdc++.so.6 (0x00007fd8bdccc000)
        librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007fd8bdac4000)
        libutil.so.1 => /lib/x86_64-linux-gnu/libutil.so.1 (0x00007fd8bd8c1000)
        libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007fd8bd6bd000)
        libgmp.so.10 => /usr/lib/x86_64-linux-gnu/libgmp.so.10 (0x00007fd8bd43c000)
        libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007fd8bd04b000)
        libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007fd8bccad000)
        /lib64/ld-linux-x86-64.so.2 (0x00007fd8be491000)
        libgcc_s.so.1 => /lib/x86_64-linux-gnu/libgcc_s.so.1 (0x00007fd8bca95000)
```

In my example here, we can see two obvious problems: 1) we get a glibc version match failure 2) we do not have `lib5info.so.6` (ncurses) on our environment.

The first error is understandable, as my mess of a computer is running ancient software:

```shell
$ lsb_release -a
No LSB modules are available.
Distributor ID: Ubuntu
Description:    Ubuntu 18.04.6 LTS
Release:        18.04
Codename:       bionic

$ ldd --version
ldd (Ubuntu GLIBC 2.27-3ubuntu1.6) 2.27
Copyright (C) 2018 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
Written by Roland McGrath and Ulrich Drepper.
```

The second problem could likely be addressed by installing ncurses globally or setting `$LD_LIBRARY_PATH` in my environment, but I'm not so convinced this is an actual solution: different applications usually call for different versions of ncurses to be installed, and it becomes a pain to deal with.

So I would rather just source these libraries from whatever nixpkgs I have instead, pinning them if truly needed.

## Finding the libraries we need

Above, we were missing `libtinfo.so.6`. This means that in some Nix derivation output there must be some path `derivation-name/lib` or something that contains `libtinfo.so.6`. It's likely we can search `'<nixpkgs>'` for its `ncurses` attribute and look at its contents and find our library there.

```shell
# let's build ncurses and see what's inside
$ nix-build '<nixpkgs>' -A 'ncurses'
/nix/store/9za4bb6zxwwz6jcd2v6dd84801gh41vj-ncurses-6.3-p20220507

# great, let's look at what's in ncurses-output/lib
$ ls $(nix-build '<nixpkgs>' -A 'ncurses')/lib
libform.so       libmenu.so.6     libncursesw.so      libpanelw.so.6
libform.so.6     libmenuw.so      libncursesw.so.6    libpanelw.so.6.3
libformw.so      libmenuw.so.6    libncursesw.so.6.3  libtinfo.so
libformw.so.6    libmenuw.so.6.3  libpanel.so         libtinfo.so.6
libformw.so.6.3  libncurses.so    libpanel.so.6       terminfo
libmenu.so       libncurses.so.6  libpanelw.so
```

There it is.

To make figuring out what we actually need easier, we can use the `patchelf` tool from Nixpkgs to also print out the needed libraries for the binary.

```shell
$ nix-shell -p patchelf
$ patchelf --print-needed zephyr
libz.so.1
libtinfo.so.6
libpthread.so.0
libstdc++.so.6
librt.so.1
libutil.so.1
libdl.so.2
libgmp.so.10
libc.so.6
libm.so.6
```

I am cheating here, but I can see that we will need gcc, z, ncurses, and gmp in this list.

We can also observe the interpreter name, which we will replace with some other interpreter from nixpkgs.

```shell
$ patchelf --print-interpreter zephyr
/lib64/ld-linux-x86-64.so.2
```

## Patching the binary

So now all we really need to do is wire this up so that the interpreter is replaced and the PITHY is set to contain the libraries we have determined are needed to run the damned thing.

```nix
let
  pkgs = import <nixpkgs> {};

  # the libraries we need
  libs = [
    pkgs.stdenv.cc.cc.lib
    pkgs.gmp
    pkgs.zlib
    pkgs.ncurses
  ];

  # we get Nixpkgs to create the library paths for us, so we don't have to wrangle them by hand
  libPath = pkgs.lib.makeLibraryPath libs;

in pkgs.runCommand "patched-zephyr" {
  # we copy the binary into the nix store for use in the derivation
  src = ./zephyr;
  buildInputs = [
    pkgs.patchelf
  ];
} ''
  mkdir -p $out/bin
  ZEPHYR=$out/bin/zephyr

  cp $src $ZEPHYR

  # we need to write to the binary
  chmod +w $ZEPHYR
  # we set the interpreter and rpath here (we could use --debug for more info)
  patchelf $ZEPHYR --set-interpreter ${pkgs.stdenv.cc.bintools.dynamicLinker} --set-rpath ${libPath}
''
```

With this fairly simple derivation we can check that the output works as we wanted:

```shell
$ nix-build
/nix/store/64qdv2pwnc3gl7f0z6q9l6mgj9hvz8q0-patched-zephyr

$ ldd ./result/bin/zephyr
        linux-vdso.so.1 (0x00007fffb29e9000)
        libz.so.1 => /nix/store/6549jyzmdk7mqv3wvrqbqjr02zjp9csw-zlib-1.2.13/lib/libz.so.1 (0x00007f4ba67b5000)
        libtinfo.so.6 => /nix/store/9za4bb6zxwwz6jcd2v6dd84801gh41vj-ncurses-6.3-p20220507/lib/libtinfo.so.6 (0x00007f4ba6741000)
        libpthread.so.0 => /nix/store/ynn1by1qdl16q6qwwh2h7zkgrn36c6i8-glibc-2.35-163/lib/libpthread.so.0 (0x00007f4ba673c000)
        libstdc++.so.6 => /nix/store/67lnvnz4qd8w3q9r5pwcm9kl5fy3b2b4-gcc-11.3.0-lib/lib/libstdc++.so.6 (0x00007f4ba6526000)
        librt.so.1 => /nix/store/ynn1by1qdl16q6qwwh2h7zkgrn36c6i8-glibc-2.35-163/lib/librt.so.1 (0x00007f4ba651f000)
        libutil.so.1 => /nix/store/ynn1by1qdl16q6qwwh2h7zkgrn36c6i8-glibc-2.35-163/lib/libutil.so.1 (0x00007f4ba651a000)
        libdl.so.2 => /nix/store/ynn1by1qdl16q6qwwh2h7zkgrn36c6i8-glibc-2.35-163/lib/libdl.so.2 (0x00007f4ba6515000)
        libgmp.so.10 => /nix/store/0zn0asff5scmj4fl0vzipmfkl440d1ib-gmp-with-cxx-6.2.1/lib/libgmp.so.10 (0x00007f4ba6475000)
        libc.so.6 => /nix/store/ynn1by1qdl16q6qwwh2h7zkgrn36c6i8-glibc-2.35-163/lib/libc.so.6 (0x00007f4ba626c000)
        libm.so.6 => /nix/store/ynn1by1qdl16q6qwwh2h7zkgrn36c6i8-glibc-2.35-163/lib/libm.so.6 (0x00007f4ba618c000)
        /nix/store/ynn1by1qdl16q6qwwh2h7zkgrn36c6i8-glibc-2.35-163/lib/ld-linux-x86-64.so.2 => /nix/store/ynn1by1qdl16q6qwwh2h7zkgrn36c6i8-glibc-2.35-163/lib64/ld-linux-x86-64.so.2 (0x00007f4ba67d5000)
        libgcc_s.so.1 => /nix/store/67lnvnz4qd8w3q9r5pwcm9kl5fy3b2b4-gcc-11.3.0-lib/lib/libgcc_s.so.1 (0x00007f4ba6170000)

$ ./result/bin/zephyr  --version
0.5.2
```

Now I'm no Linux or C expert but it looks like everything has been wired up here successfully.

## Conclusion

Hopefully this has shown that working with binaries can be made fairly easy with Nix by patching the binaries.

I think that this article should also help you investigate issues with running programs inside Docker containers, and what you should look at when trying to debug Docker builds.

"Why not static binaries upstream???" can likely be answered by your own readings and thinking about the problem.
