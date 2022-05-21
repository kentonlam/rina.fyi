---
title: primitive and tail recursion
date: 2022-05-15
author: Rina
excerpt: asdf
---
# primitive and tail recursion

We were given the claim that "any primitive recursive function is tail recursive", along with some Haskell code to support the claim. This is an (unprompted) response including an exploration of the problem, the claimed solution, and related topics.

## definition

A primitive recursive function is defined inductively as a function $f : B \to \mathbb N \to A$ which can be written in the following form.
$$
\begin{aligned}
f(b,0) &= f_0(b) \\ 
f(b, \operatorname {Suc} n) &= f_s(b,n, f(b,n))
\end{aligned}
$$