---
title: quines
date: 2023-03-11
author: Kait
title: anatomy of a quine
description: a quine is a program which, when run, prints itself.
draft: true
---
# quine

A _quine_ is a program which prints its own code.
Quines are fascinating.
This self-contained, minimal task is a natural problem to pose in any programming language, even the most barebones ones.

My recent dive into quines is due to Ken Thompson.
One of computing's great ancients, <!--Thompson is responsible for much of Unix's foundational designs, as well as precursors to many programming languages, tools, and standards we know today.
In 1983, --> he and Dennis Ritchie were awarded the Turing Award for their foundational work on Unix.
For such a prestiguous award, one is usually allowed a speech.
Ken Thompson's speech was titled _Reflections on Trust_.

In this, he talks about a supply chain attack on compilers&mdash;the starting point of every program.
In particular, the duties we entrust to these programs which write programs and how that trust can be subverted.

On quines, he writes

> More precisely stated, the problem is to write a
source program that, when compiled and executed, will
produce as output an exact copy of its source. If you
have never done this, I urge you to try it on your own.
The discovery of how to do it is a revelation that far
surpasses any benefit obtained by being told how to do
it.

Though I'd known of quines, this is what prompted me to try one myself.
As promised by our Unix-writing mentor, it was great fun and good food for thought.


# can we build it

I hope you gave it a try!
Here is what I did with my attempt.

We'll use the universal language of data science and machine learning: Python.
For our first try, there's no need to be overly clever.
We'll start with the bare minimum, a single print call.
```python
print("
```
Whatever do we print?
Well, the problem answers this for us.
We print the program itself, so everything from the start of the line.
```python
print("print(
```
Here, we see our first obstacle.
The next character to be printed is `"`, but printing this will need an _escape_.
If we continue in this way, we're forced to add and continue adding backslashes.
```python
print("print(\"print(\\\"print(\\\\\\\"
```
Now at this point, if this was mathematics, we could slap $\cdots$ on the end and we'd be done.
Unfortunately, we'll have to do a some work to make a solution that fits on a conventional hard drive.

# yes we can

What we really need is some way of tying the know&mdash;some
way of feeding this program into itself.
One way of doing this is to use a _variable_.


Let's try and make a quine.
For the moment, we'll use the universal language of data scienve and machine learning: Python.

We'll definitely need a print to output the text.
That sounds obvious enough, so we can start with that.

```python
print("print(...)")
```

We have run into a problem

The goal is to, somehow, feed a program back into itself.

some kind of substitution

some kind of representation of strings and special characters


# fixing points

this begins to look a lot like fixed points

computability theory
fixed points

idk
