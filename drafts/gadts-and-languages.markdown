---
title: GADTs and EDSLs
category: haskell
tags: Haskell, GADTs
---

## Reflect types

One of the most important things is to create a type class which reflects the types of the primitive
types of your language. Otherwise you won't be able to do anything with them. Say you want to write
a compiler from an EDSL (with a shallow embedding of the types) to one with a deep embedding of the types.
