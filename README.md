# rina

[![Build and deploy to Pages](https://github.com/katrinafyi/rina.fyi/actions/workflows/pages.yml/badge.svg)](https://github.com/katrinafyi/rina.fyi/actions/workflows/pages.yml)

This site is built with Hakyll, with package management by Haskell's stack tool.
It should be sufficient to run
```bash
stack build
```
to initialise the project and its dependencies.
The first time, this will take a while because of Pandoc.

Once built, Hakyll is used by executing the built binary (unlike other tools which use a separate external tool, the Hakyll compiler is this binary itself).
```bash
stack run
```
should suffice.

## nix

If you're lucky enough to use the Nix package manager, this is so much faster:
```bash
nix build .
```
Then, run with `LC_ALL=C.utf-8 nix run .`.
