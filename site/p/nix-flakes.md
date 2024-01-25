---
title: Nix flakes from up close
date: 2024-01-25
author: Kait
description: pointers to resources which helped me learn Nix flakes.
---
# Nix flakes from up close

Unlike most aspects of Nix, flakes have a substantial amount of written content about them.[^1]
However, I spent my first few months of Nix warily avoiding flakes. 

[^1]: Reading about Flakes felt a lot like reading Haskell monad tutorials.

Early on, I had come about the overlay method for extending nixpkgs.
This was fairly easy to me.
Fixed points were familiar and overlays were trivially composable into one fixed point.
This worked well until the time arrived to distribute overlays.
It could be done, with an `import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; }`,
but this was fragile and relied on each user's nixpkgs channel. 

Nix flakes were the obvious solution to this.
Fundamentally, they provide a consistent entry point into a Nix expression from the disordered outside world.
By pinning inputs (e.g. nixpkgs) to particular revisions and committing the lock file,
everyone using the flake gets an identical package derived from the same upstream nixpkgs.
This was a huge boon for reliability and testing but it was not instantly mastered.

When learning new things, I generally want to know two things:
- how does this relate to things I already know?, and
- what is the minimal (or most abstract) example of such a thing?

This is the path I have walked.

## resources

### [_Flakes aren't real and cannot hurt you: a guide to using Nix flakes the non-flake way_](https://jade.fyi/blog/flakes-arent-real/) (2024)

The resource I'd like to credit for my aha! about flakes, and the reason I'm writing this page,
is _Flakes aren't real and cannot hurt you_ by Jade Lovelace.
Beyond its enticing title, the post does a wonderful job of distilling Flakes down into their most primitive form.
It debunks bold claims about Flakes made by other more SEO-optimised tutorials and, along the way,
showed me how to dovetail the overlays I already had into this new infrastructure. 

The minimal examples in the post highlight that Flakes are not especially magical.
Syntactically, they are regular Nix expressions which evaluate to an attrset.
Their usefulness comes from the [nix(1)](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix) tooling
which treats such an attrset in a unique way.
This, however, is also not new in Nix;
package derivations and NixOS modules also have their own special treatments, and
flakes are just another kind of attrset.

### [flake schema](https://nixos.wiki/wiki/Flakes#Flake_schema) and [`nix flake` reference](https://nixos.org/manual/nix/stable/command-ref/new-cli/nix3-flake.html)

With an understanding of flakes at the micro level, the wiki and reference documentation become much more approachable.
In particular, the schema section of the wiki page does a good job of linking each output attribute to the flake command which uses it.
At this point, `nix flake show` was very helpful to try.
It pretty-prints your flake's contents and performs some schema checking while doing so.

### [_Why you don't need flake-utils_](https://ayats.org/blog/no-flake-utils/) (2023)

This page succintly answers a question I had given the prevalence of flake-utils in prior art.
Importing a helper library runs counter to my desire to keep flakes as minimal as possible,
so I have a chance of understanding its workings.
Fortunately, this post shows how the same thing can be achieved by your hand-written Nix code,
the way it should be.
