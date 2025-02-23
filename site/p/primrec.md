---
title: primitive and tail recursion
date: 2022-05-15
author: Rina
description: a comment on tail recursion, with particular attention to Haskell.
draft: true
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

here, $f_0$ and $f_s$ must also be primitive recursive functions with the following types: $f_0 : B \to A$ and $f_s : B \to \mathbb N \to A \to A$.

This differs slightly from the notation in the claim. We name the sub-functions $f_0$ and $f_s$ because of their role in defining the base zero case and the successor case, respectively. We also use the successor function to make explicit the recursive structure.

If we look closely at the recursive case, we will notice a pattern. Clearly, there is recursion at work but the pattern of the recursion is quite specific: it uses a function ($f_s$) to combine the current value with the recursive value. This should be familiar because it appears in many types, for example in a possible implementation of `map` for List.

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs
```

Though it's less obvious, this takes the current value `x` and combines it using `:` with the recursive call on the rest of the list. The general pattern for this is called a _fold_. In Haskell, this is defined in the Foldable typeclass.

```haskell
class Foldable t where
	foldr :: (a -> b -> b) -> b -> t a -> b
```

We will come back to this later after looking at the claim.

## claim

The code given to support the claim is below. Firstly, for the case where $B$ is not needed.

```haskell
t :: (Int -> a -> a) -> a -> Int -> a
t h u 0 = u
t h g y = t h (h (y-1) g) (y-1)
```

We will take this and tweak it to more closely match our definition above.

```haskell
tailrec :: (Int -> a -> a) -> a -> Int -> a
tailrec fs f0 0 = f0
tailrec fs f0 n = tailrec fs (fs (n-1) f0) (n-1)
```

There is also a version for where the argument $B$ is included. This is done using a list.

```haskell
tl :: [b] -> ([b] -> Int -> ([b] -> a) -> a) -> ([b] -> a) -> Int -> a
tl xs h u 0 = u xs
tl xs h g y = tl xs h (\x -> h xs (y-1) g) (y-1)
```

Here, a few things are interesting but first, we will break down the accursed type signature by matching it with the primitive recursive definition.

```haskell
tl ::
	[b] ->			-- initial context
	(				-- f_s function (?)
		[b] -> 			-- context injected
		Int -> 			-- n of current call
		([b] -> a) ->	-- result of recursive subcall (?)
		a				-- result of combining
    ) ->
    ([b] -> a) -> 	-- f_0 function
    Int -> 			-- natural number to evaluate function at
    a				-- result of tail recursive function
```

It is not clear why `([b] -> a)` is needed instead of `a` in the $f_s$ parameter, since this should represent the recursive call's result in the definition. It is also not clear why `\x ->` is used in the recursive call, then `x` is not used in the expression.

After some thinking, it appears to be this is done so it matches the arguments of the outer `tl` function, both taking a `[b] -> a` function. This shouldn't be needed because we always have access to a `[b]` as an argument so can evaluate it to `a`.

We will rewrite this slightly to replace the `[b] -> a` argument and also rename the variables. This lets it more closely match the mathematical definitions. We will also replace the `[b]` with a more general `b` because the structure is not needed.

```haskell
tailrec' :: b -> (b -> Int -> a -> a) -> (b -> a) -> Int -> a
tailrec' b fs f0 0 = f0 b
tailrec' b fs f0 n = tailrec' bs fs (\b -> fs b (n-1) (f0 b)) (n-1)
```

Additionally, we should be able to write this version with `[b]` using just `tailrec` and partially applying functions.

```haskell
tailrec'' :: [b] -> ([b] -> Int -> a -> a) -> ([b] -> a) -> Int -> a
tailrec'' bs fs f0 n = tailrec (fs bs) (f0 bs) n
```

It is easy to see the types match up and hence the function is (obviously) correct.

### examples

We will now look at the given examples which we write verbatim below. Note that these use the first versions of `t` and `tl`.

```haskell
-- factorial
fact = t (\x -> (\y -> (x+1)*y)) 1
-- 7^x
seventothe = tl [7] (\xs -> \y -> \z -> (xs !! 0) * (z [7])) (\xs -> 1)
-- x^y
exponen = \x -> tl [x] (\xs -> \y -> \z -> (xs !! 0) * (z [x])) (\xs -> 1)
```

We will skip looking at each example in detail. Some notes:

- Use of many `\` expressions makes the lambda functions very long.
- Use of `[b]` as a list makes using the argument inconvenient.
- The inner `[b] -> a` parameter in `tl` makes it difficult to use. In the last two cases, although the function is a constant it needs to be evaluated with some list.

## the difference

We return to our original definition of a primitive recursive function $f : B \to \mathbb N \to A$.
$$
\begin{aligned}
f(b,0) &= f_0(b) \\
f(b, \operatorname {Suc} n) &= f_s(b,n, f(b,n))
\end{aligned}
$$
In fact, we will remove the $b$ parameter by assuming partial application is possible. Now, we have $f : \mathbb N \to A$. The $B$ parameter is also removed from $f_0$ and $f_s$.
$$
\begin{aligned}
f(0) &= f_0 \\
f(\operatorname {Suc} n) &= f_s(n, f(n))
\end{aligned}
$$
We can write this in Haskell as below. Note that $n-1$ is used because we don't have the $\operatorname{Suc} n$ pattern matching on the left hand side.

```haskell
primrec :: (Int -> a -> a) -> a -> Int -> a
primrec fs f0 0 = f0
primrec fs f0 n = fs (n-1) (primrec fs f0 (n-1))
```

Now recall the definition of `t` in the claim. We will copy our renamed `tailrec` version here.

```haskell
tailrec :: (Int -> a -> a) -> a -> Int -> a
tailrec fs f0 0 = f0
tailrec fs f0 n = tailrec fs (fs (n-1) f0) (n-1)
```

_Now_, observe the subtle difference between these two. They both recurse and use $f_s$ to combine results but the difference is in the order results are combined. `primrec` combines it last, combining with $n-1$ after the recursive call. `tailrec` combines it before the recursive call with `fs (n-1) f0`.

We can make this more obvious by calling the two functions with arguments which will generate a string. This should make it easier to see the order $f_s$ is evaluated as well as its arguments.

```haskell
fs n a = "fs (" <> show n <> ") (" <> a <> ")"
f0 = "f0"

main = do
	putStrLn $ primrec fs f0 3
    putStrLn $ tailrec fs f0 3
```

Executing this gives us the following output:

```
fs (2) (fs (1) (fs (0) (f0)))
fs (0) (fs (1) (fs (2) (f0)))
```

This makes it clear why the tail recursive form doesn't work with non-commutative $f_s$ functions.

## the point

Tail recursion is commonly used in procedural languages because it lets the compiler perform _tail call optimisation_. This is needed because function calls are usually very slow. Tail call optimisation allows a recursive call to _reuse_ the previous call's stack frame, avoiding the time and memory overhead of allocating a new frame. However, this can only be done in very specific circumstances. The recursive call needs to match the structure of the original call, possibly with some modified arguments.

However, this is much less of a concern in Haskell for a couple of reasons. Firstly, being a functional programming language, Haskell is centred around functions and so, function calls are extremely quick in almost all cases. Secondly and more importantly, naïve tail recursion does not provide the same benefits as ordinary tail call optimisation. This is because of laziness.

Haskell is a lazy language. _Everything_ is lazy unless explicitly made strict. This means that even if something appears tail recursive in the traditional sense, it could still be building up a large unevaluated calculation. See [Strictness](https://wiki.haskell.org/Performance/Strictness) on HaskellWiki and [Stack Overflow](https://stackoverflow.com/a/13052612).

Hence, blindly rewriting Haskell functions to tail recursion for "tail" "call" "optimisation" is a bad idea. In the majority of cases, it's a better idea to use more idiomatic higher order functions for recursion and traversal. Where optimisation is needed, this should be done with an understanding of Haskell's evaluation model and performance characteristics.

## beyond the point

These are some more notes which link this notion of "recursion" with some other things I've encountered. These will be more brief.

### folds

The different evaluation order of `primrec` and `tailrec` given above corresponds to the two directions of folding. Specifically, you can `foldr` which combines the first element with the recursive subresult, or `foldl` which combines the recursive subresult with the last element.

To highlight this point, I would've liked to write a Foldable instance for Int but unfortunately we can't do that because Foldable needs a higher order type (for example, you say `Foldable []` to say `[a]` is foldable for all `a`).

Instead, we observe that the recursive evaluation passes decreasing integers to the `f_s` function. With this in mind, we will convert integers to a list the fold over that list!

```haskell
intToList :: Int -> [Int]
intToList 0 = []
intToList n = [n-1,n-2..0]
-- start at n-1 because of primrec and tailrec definition
```

We will use this along with the foldr and foldl functions to rewrite tailrec and primrec. First, we show foldl and foldr for the `[Int]` type to see their differences.

```haskell
foldr :: (Int -> b -> b) -> b -> [Int] -> b
foldr f b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

foldl :: (b -> Int -> b) -> b -> [Int] -> b
foldl f b [] = b
foldl f b (x:xs) = foldl f (f b x) xs
```

We note that foldl is a sort of tail recursion and foldr is more closer to the primitive recursion definition. And hence, we will write this:

```haskell
primrecfold fs f0 n = foldr fs f0 (intToList n)
tailrecfold fs f0 n = foldl (flip fs) f0 (intToList n)
```

We can check that these produce the same orders as their original versions.

```haskell
main = do
  putStrLn $ primrec fs f0 3
  putStrLn $ primrecfold fs f0 (3::Int)
  putStrLn $ tailrec fs f0 3
  putStrLn $ tailrecfold fs f0 (3::Int)
```

```
fs (2) (fs (1) (fs (0) (f0)))
fs (2) (fs (1) (fs (0) (f0)))
fs (0) (fs (1) (fs (2) (f0)))
fs (0) (fs (1) (fs (2) (f0)))
```

#### strictness

One might also wonder which of foldl and foldr is faster in particular cases. Of course, this will depend on the functions used so we will consider adding up a list. Intuitively, this should be faster with tail recursion (and thus foldl) because it's only a single number while recursing.

```haskell
import Data.Foldable
import Data.Time.Clock

time :: IO () -> IO ()
time f = do
  putStrLn "start..."
  t0 <- getCurrentTime
  f
  t1 <- getCurrentTime
  putStr "... end in "
  print $ diffUTCTime t1 t0

main = do
  time $ print $ foldr (+) 0 [1..3000000]
  time $ print $ foldl (+) 0 [1..3000000]
```

They turn out to be extremely close.

```
start...
4500001500000
... end in 3.218930618s
start...
4500001500000
... end in 3.392049483s
```

This is because of laziness. With Haskell's lazy evaluation models, both are folds are doing almost the same thing; they're just accumulating a computation of `+` which only gets evaluated at the end when the result is needed to print.

Luckily, Data.Foldable also provides `'` variants of these two functions which are strict in the accumulated value. Changing it to this makes `foldl'` an order of magnitude faster than `foldr'`.

```
start...
4500001500000
... end in 2.308533222s
start...
4500001500000
... end in 0.573823784s
```

### reader monad

There's a funny pattern in OOP or Java land called *dependency injection*. It's the idea that classes which depend on some value can, with the right library support, pull that value out of thin air from some context. There is a corresponding pattern in Haskell which is much neater and more concrete.

Remember up above when we wrote this.

```haskell
tailrec'' :: [b] -> ([b] -> Int -> a -> a) -> ([b] -> a) -> Int -> a
tailrec'' bs fs f0 n = tailrec (fs bs) (f0 bs) n
```

It sure looks like there's some dependency on `bs` here. In fact, `tailrec''` doesn't really need that argument at all; it only passes it to the other functions. It would be nice if we could write `tailrec''` without mentioning this dependency at all.

It turns out we can do that. This idea of a context is the type `(->) a` or the *Reader monad*. This is instantiated with another type, say `[b]`, and defines an environment where we assume `[b]` is available. We will skip the details and just show it in use.

We could've written `tailrec''` like so. We had to move the injected parameter to the end. The `fs' <- fs` notation is used to obtain a version of `fs` with the parameter already partially applied.

```haskell
tailrec''' :: ([b] -> Int -> a -> a) -> ([b] -> a) -> Int -> [b] -> a
tailrec''' fs f0 n = do
	fs' <- fs
	f0' <- f0
	pure $ tailrec fs' f0' n
```

In fact, we can do more. Since there's a `pure` on the last line, this can be rewritten using Functor and Applicative functions.

```haskell
tailrec'''' :: ([b] -> Int -> a -> a) -> ([b] -> a) -> Int -> [b] -> a
tailrec'''' fs f0 n = tailrec <$> fs <*> f0 <*> pure n
```

Brilliant!

### type theory

In Martin-Löf Type Theory, types are defined by specifying four sets of rules:

- formation rules describe how the type can be constructed,
- introduction rules describe how you can obtain an element of that type,
- elimination rules describe how you can use an element of that type, and
- computation rules relate the introduction and elimination rules.

This notion of primitive recursion is so pervasive that it turns up as the elimination rule for natural numbers. Here, $p_0$ and $p_S$ are analogous to $f_0$ and $f_s$ in our definition. $\Pi$ defines a dependent function taking an argument $n : \mathbb N$.

$$
\frac{\begin{align*}
\Gamma, n : \mathbb N &\vdash P(n) \text{ type} \\
\Gamma&\vdash p_0 : P(0_{\mathbb N}) \\
\Gamma &\vdash p_S : \Pi_{(n : \mathbb N)}(P(n) \to P(\operatorname{succ}_{\mathbb N}(n)))
\end{align*}
}{
\Gamma \vdash \operatorname{ind}_{\mathbb N}(p_0, p_S) : \Pi_{(n : \mathbb N)} P(n)
}
\mathbb N\text{-ind}
$$

Moreover, the computation rules specify that it behaves identically to the primitive recursive definition. First for the base case,
$$
\frac{\begin{align*}
\Gamma, n : \mathbb N &\vdash P(n) \text{ type} \\
\Gamma&\vdash p_0 : P(0_{\mathbb N}) \\
\Gamma &\vdash p_S : \Pi_{(n : \mathbb N)}(P(n) \to P(\operatorname{succ}_{\mathbb N}(n)))
\end{align*}
}{
\Gamma \vdash \operatorname{ind}_{\mathbb N}(p_0, p_S, 0_{\mathbb N})~\dot=~ p_0 : P(0_{\mathbb N})
}
$$

and also the inductive case,
$$
\frac{\cdots}{
\Gamma, n : \mathbb N \vdash \operatorname{ind}_{\mathbb N}(p_0, p_S, \operatorname{succ}_{\mathbb N}(n))~\dot=~
p_S(n, \operatorname{ind}_{\mathbb N}(p_0, p_S, n)) : P(\operatorname{succ}_{\mathbb N}(n))
}.
$$
