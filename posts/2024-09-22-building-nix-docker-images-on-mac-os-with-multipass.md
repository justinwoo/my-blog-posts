# Building Nix Docker images on Mac OS with Multipass

Recently I've had my own exposure to the problem nobody likes in Nix: trying to build Docker images using nixpkgs dockerTools in Mac OS.

I was reminded of the existence of [Multipass](https://github.com/canonical/multipass/) by a comment on the NixOS Discourse (that I can no longer find), and this ended up being easy enough to use.

## My example scenario

I want to build a Docker image that extends some other base image I'm creating. The definition of this image is fairly simple:

```nix
{ system ? builtins.currentSystem
, pkgs ? import ./nix/pinned-24_05.nix { inherit system; }
}:

let
  base-image = pkgs.dockerTools.buildImage {
    name = "hello-docker";

    copyToRoot = pkgs.buildEnv {
      name = "image-root";
      paths = with pkgs; [
        coreutils
        bashInteractive
      ];
      pathsToLink = [
        "/"
      ];
    };
  };

in
pkgs.dockerTools.buildLayeredImage {
  name = "hello-layered";

  fromImage = base-image;

  contents = [
    pkgs.hello
  ];

  config = {
    Cmd = "hello";
  };
}
```

You might see the problem that arises quite easily if you have dealt with building Docker images before, or you have dealt with supplying `system` as a parameter: my system is `aarch64-darwin` or something, and the image we want to build and run is `aarch64-linux` or so. If you try to build this image and load it up from Mac OS from start to finish, you will get errors about the wrong executable format, as you'd expect.

## Enter Multipass

I'd honestly forgotten about this project, but it's basically a VM manager for a bunch of Ubuntu images. After installing it whatever way you wish, you will need to then launch an instance to work with:

```
macos $ multipass launch --name testbuilder
```

You may also need `--disk 20G` and such to allocate more space to work with.

Then you will need to mount the directory you want to deal with into the instance:

```
macos $ multipass mount $(pwd) testbuilder
```

Afterwards, you can shell in.

```
macos $ multipass shell testbuilder
```

Since this is just a plain base image, you will need to do the Nix installation manually:

```
mp-ubuntu $ sh <(curl -L https://nixos.org/nix/install)
mp-ubuntu $ . /home/ubuntu/.nix-profile/etc/profile.d/nix.sh
```

## Building the image

Once you get into your project, you can build away in your new _architecture_ + Linux system. Keep in mind that nix-build will create a result symlink into the Nix store of the VM, so you will need to copy the image tarball into the directory directly.

```
mp-ubuntu $ cp $(nix-build -j 20) result.tar.gz

Adding base layer 1 from 3d7859d4d8d8279037271f0a03ff5d5e372e4c319d0416d775c529da695064c3/layer.tar
Creating layer 2 from paths: ['/nix/store/p3vmkyxfvd8flmgi6jr38vwl48lz96wq-libunistring-1.1']
Creating layer 3 from paths: ['/nix/store/wvcr4k2l6ba83y80pyvjnisnpj4c75wy-libidn2-2.3.7']
Creating layer 4 from paths: ['/nix/store/ikj27dh6lrrarmnmvay26d6b246nykz4-xgcc-13.2.0-libgcc']
Creating layer 5 from paths: ['/nix/store/27fg1mkiymj2b344j80kygsbxfcdl5qi-glibc-2.39-52']
Creating layer 6 from paths: ['/nix/store/h8j29rxrqk4s9nb2issyybrbfjcbfdmc-hello-2.12.1']
Creating layer 7 with customisation...
Adding manifests...
Done.
```

This with this, we have our image we can load up and use from the host side.

## Consuming the image

Now we can load up the image and run it, as expected:

```
$ docker load < result.tar.gz
Loaded image: hello-layered:jr00mj15ckmpri8237ygi8fvywvg7ndd
$ docker run -it hello-layered:jr00mj15ckmpri8237ygi8fvywvg7ndd
Hello, world!
$ docker run -it hello-layered:jr00mj15ckmpri8237ygi8fvywvg7ndd bash
bash-5.2# ls
```

## Considerations

This is likely one of the least efficient ways of doing a build on Mac OS, and likely any sophisticated build will be OOM killed, but with the right flags and provisioning of resources, this may at least help providing a local feedback loop when trying to either purely run Nix instantiations or building smaller images.

## Future Improvements

A Nix formula for Multipass would be quite convenient, and I think someone out there must have already made one.

## Links

- Multipass: <https://github.com/canonical/multipass/>
- Demo repo (code only): <https://github.com/justinwoo/macos-nix-docker-build-multipass>
