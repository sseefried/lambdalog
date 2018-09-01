---
title: Gabriel Gonzalez's notes on Stack, Cabal and Nixpkgs
category: notes
tags: Haskell, Cabal, Stack, Nix, build systems
---

[Gabriel Gonzalez](http://www.haskellforall.com/) recently
[tweeted](https://twitter.com/GabrielG439/status/1025206860301840384)
about the history of [Stack](https://docs.haskellstack.org/en/stable/README/)
which I thought were worth recording for posterity here.

> When `stack` was first introduced the killer features were:
>
> * declarative dependency specification (i.e. `stack.yaml`)
> * isolated project builds
>
> `cabal new-build` was when `cabal-install` began to support the above two
> features (`cabal.project` being analogous to `stack.yaml`)Gabriel Gonzalez added,
>
> Also, once Nixpkgs added Haskell support that also offered a third approach for
> isolated builds with declarative dependencies
>
> The combination of `cabal new-build` + Nix's Haskell integration is what caused
> `stack` to lose its competitive advantage on that front
>
> A small advantage of `stack` was built-in support for using Stackage resolvers
> to select a working package set en masse
>
> However, you could reuse that within a Cabal project by saving
> https://www.stackage.org/lts/cabal.config to your project
>
> This compatibility was an intentional goal of @snoyberg
>
> Stackage was probably the single best outcome of the `stack` project because it
> benefited all three of `stack`, `cabal-install` and Nixpkgs (which closely
> tracks the latest Stackage resolver, too)
>
> Before Stackage getting a working build of, say, `lens` or `yesod` was a
> nightmare
