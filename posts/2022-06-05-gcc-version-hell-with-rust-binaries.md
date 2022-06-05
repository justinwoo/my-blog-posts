# GCC Version Hell with Rust Binaries

I'm not convinced this has a good solution.

See also: <https://kobzol.github.io/rust/ci/2021/05/07/building-rust-binaries-in-ci-that-work-with-older-glibc.html>

## Verification of the problem

```console
$ ./result//bin/update-fetch
./result//bin/update-fetch: /nix/store/vaf92wc23m067q9aig6zjkcnjvrg768n-glibc-2.32-46/lib/libc.so.6: version `GLIBC_2.33' not found (required by ./result//bin/update-fetch)
./result//bin/update-fetch: /nix/store/vaf92wc23m067q9aig6zjkcnjvrg768n-glibc-2.32-46/lib/libc.so.6: version `GLIBC_2.34' not found (required by ./result//bin/update-fetch)
```

GLIBC version on origin is too recent.

```console
$ objdump -T ./result/bin/.update-fetch-wrapped  | grep GLIBC_2.33
objdump: warning: ./result/bin/.update-fetch-wrapped: unsupported GNU_PROPERTY_TYPE (5) type: 0xc0008002
0000000000000000      DF *UND*  0000000000000000  GLIBC_2.33  stat64
0000000000000000      DF *UND*  0000000000000000  GLIBC_2.33  fstat64
```

Among others.

Check host ldd:

```console
$ ldd --version
ldd (Whatever GLIBC 2.27-whatever) 2.27
```

Probably origin has 2.34+.

## Naive solution

Add glibc to buildInputs.

## Observations

```console
$ nix-shell
$ which ldd
/nix/store/57g080n6rmzzq93gm5m9yc87zvqlh4yi-glibc-2.32-46-bin/bin/ldd
$ ldd --version
ldd (GNU libc) 2.32
$Copyright (C) 2020 Free Software Foundation, Inc.
This is free software; see the source for copying conditions.  There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
$Written by Roland McGrath and Ulrich Drepper.
$ objdump -T target/release/update-fetch | grep GLIBC_2.3
# no entries for 2.33 and whatever
```

Should be no more problems.

## Takeaway

I should have used something else.