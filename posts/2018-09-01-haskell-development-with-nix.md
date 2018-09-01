---
title: Haskell development with Nix
category: notes
tags: Haskell, Nix, build systems
---

The [Nix](https://nixos.org/nix/) package manager home page has this to say:

> Nix is a powerful package manager for Linux and other Unix systems that makes
> package management reliable and reproducible. It provides atomic upgrades and
> rollbacks, side-by-side installation of multiple versions of a package,
> multi-user package management and easy setup of build environments.

I like Nix because it allows me to:

- create reproducible development environments
  that do not interfere with other software installed on my computer
- specify the entire stack of software dependencies that my project has,
  including system libraries and build tools, using a single language:
  _Nix expressions_.

Nix has the advantage that it doesn't just tame Haskell's dependency hell but
also that of the entire system. Unfortunately, there is quite a lot to learn
about Nix before one can use it effectively. Fortunately, [Gabriel
Gonzales](http://www.haskellforall.com/) has written a great guide on how to use
Nix for Haskell development. I've also included a link to an
[addendum](https://twitter.com/GabrielG439/status/1033922517541380096) Gabriel
is going to add to the original guide.


- [Nix and Haskell in production](https://github.com/Gabriel439/haskell-nix).
  This is a detailed guide on how to use Cabal + Nix to create completely
  reproducible builds.

- [How to fetch Nixpkgs with an empty NIX PATH](https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH).
  A short addendum on how to remove impure references to `NIX_PATH`, this making
  your builds even more reproducible.

- [NixOS in production](http://www.haskellforall.com/2018/08/nixos-in-production.html).
  This post is not Haskell specific but shows you how to build a _source free_
  Nix binary closure on a build machine and then deploy it to a production machine.
  This is fantastic workflow for space/computing-limited production machines
  e.g. t2.micro instances on Amazon AWS.


