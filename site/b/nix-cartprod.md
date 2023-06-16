---
title: cartesian product (nix flavour)
date: 2023-06-14
author: Kait
description: a code example and discussion about nix
---

# cartesian product (nix flavour)

The set theoretic [Cartesian product](https://en.wikipedia.org/wiki/Cartesian_product) of two sets is the set of pairs. This is written
$$
A \times B = \{(a,b) ~|~ a \in A, b \in B\}.
$$
This is generalised by chaining &times; symbols,
which will result in longer tuples without any surprises.

In programming, it's convenient to take the Cartesian product of any number
of items, and this is called the _n-ary Cartesian product_.
To do so, we'll need a way to hold _n_ many items and this is most naturally a list[^lists].

[^lists]: Of course, this is not fully precise as a list can have any length and there's nothing constraining it to be the correct length. To fix this would require [dependent types](https://en.wikipedia.org/wiki/Dependent_type) which is too complex for most programming languages to support.

The Nix language code to do this is here.
The `cartProd` function takes a list of lists and returns the Cartesian product of all the inner lists.
```nix
let
  # given a string name and value, returns an attribute set { name = value; }.
  makeAttr = name: value: builtins.listToAttrs [(lib.nameValuePair name value)];
  # reduces a list of attributes into a single attribute, concatenating values.
  concatAttrs = lib.foldAttrs lib.concat [];
  # given list of lists, returns the cartesian product
  cartProd = xs:
    let
      attr = i: n: makeAttr (lib.fixedWidthNumber 5 i) n;
      a = lib.imap0 attr xs;
      b = lib.cartesianProductOfSets (concatAttrs a);
    in
      builtins.map builtins.attrValues b;
in
  ...
```
It works as promised:
```
nix-repl> :p cartProd [ [0] [1 2] [3 4]]
[ [ 0 1 3 ] [ 0 1 4 ] [ 0 2 3 ] [ 0 2 4 ] ]
```
And so we are done.

## bad code

It doesn't take very much examination to see that this code is awful.
Amongst its helper functions, we perform a ritual of converting the list of lists into attribute sets (keyed by string representations of numbers) and then back into the desired lists of lists.

It could be better, but there is a problem with the Nix language.
This is a slide from the multiply-skilled [Xe Iaso's talk](https://xeiaso.net/talks/nixos-pain-2021-11-10).

![the language is not the package manager is not the os is not the language. however, nix is the language, the package manager, and the os.](assets/xe-iaso.avif)

Nix (in general) is unarguably a step in the right direction for declarative environments,
but Nix (the language) has its shortcomings.
It promises the power and flexibility of a functional programming language,
and yet it feels like the Bash of the package management.
It's specialised and fit-for-purpose, and once
you step outside the intended path and it quickly falls apart.

This is evident in how the documentation consistently focuses on its uses rather than the language itself.
The [standard library](https://github.com/nix-community/nixpkgs.lib) is never mentioned or documented aside from [one third-party page](https://teu5us.github.io/nix-lib.html).
The builtins provided are both scarce and haphazard, not fitting together as you'd expect and missing many useful general-purpose operations.
This leads to silliness like we have in `cartProd` above.

## end

I think the points I've expressed are because of my background
with general purpose functional languages;
I expected the Nix language to match their expressiveness.
Despite this all, the benefits are worth the inconvenience.

Nix has charmed me like React and Haskell have done in the past.
It's a breath of fresh air amongst a crowd of approximately-the-same package managers, and a confident step towards more consistent and reliable software processes.
This is surely a good thing and I look forward to how it grows :\)

---

## the silly

This is all so ridiculous.
The goal of all the work above is to be "more efficient" by using the builtin Cartesian product.
It does some (I assume) clever things with attribute sets to avoid memory operations.
However, I have decided we don't need that for the use in our Nix config.

Here is a much simpler Cartesian product function:
```nix
cartProd =
  with lib; with builtins;
  foldl (tls: x: concatMap (tl: map (h: tl ++ [h]) x) tls) [[]];
```
And there we go.
