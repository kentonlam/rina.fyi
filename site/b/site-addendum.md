---
title: the site itself (addendum)
date: 2022-05-26
author: Kait
description: more on our goals while making the site.
---

# the site itself (addendum)

We spent a lot of time (probably too much) in [/b/site](/b/site)
talking about _what_ we did but not enough about _why_ we did it.
In particular, I want to talk about some goals and visions we had for the site.

Initially, we didn't have much in mind;
the website was really just a placeholder to stake a claim on a corner of the web.
The first version with specific goals was the Python for rina.fyi.

The generator was called `gen` (very creative) and this was the short design notes
I wrote for it.
You can see that the idea was very simple.
So simple that surely Python would be enough.
But, of course, the devil is in the details.

> # gen
> We will design a very simple static site generator to do exactly what we need and no more. This will tentatively be called **gen**.
>
> ## Main goals
>
> - ✅ Compile Markdown to HTML with pandoc.
>   - ✅ With syntax highlighting for code.
>   - ✅ With KaTeX for displaying maths.
> - ✅ Generate pages which list other pages.
> - ✅ Basic templating.
> - 😕 Obtain date from file name or otherwise.
>
> ### Details
>
> - 😕 Type annotations throughout, as much as possible.
> - 😕 Class-based interface with implicit context and procedural mutation of the context.
> - ✅ Matcher classes for matching paths by glob, file extension, and others.
> - ✅ Files will be represented as a class storing its input path, output path, data, format, and other metadata.
> - ✅ Transformer classes which can be applied to files such as Pandoc.
>
> This interface is meant to somewhat resemble Hakyll.
>
> ## Secondary goals
>
> - ✅ Table of contents extracted from Pandoc output.
>
> ## Stretch goals
>
> - Paginated list pages.
> - General recursive templates.
>
> ## Non goals
>
> - Compile speed.
> - Configuration file.
> - Manual file parsing.

Above, I've put ✅ and 😕 next to things we completed and kind of completed.
Everything else was not done.

Most of the ticks in the goals came easily from Pandoc.
The type annotations and class structure were where we really struggled.
We had no idea how fast it would become unmanageable, despite our best efforts.

The classes quickly evolved into our worst fears.
Mutable states and reference aliasing were everywhere.
Partial copies and deep copies mixed into one inscrutable monstrosity.
We were lucky to get as far as we did. It was entirely too much.

On the verge of self-destruction, we gave it up and threw it all away.
We paused and took a look at what we had.
The orginal design for `gen` was sound.
The goals of simplicity and easy listings were things we wanted.
Then, it was rewritten in Haskell with Hakyll.

After all that, I think we have these guiding principles.
They're a mix of the `gen` goals and things floating in the back of my head.

- Simple pure HTML+CSS with minimal Javascript.

  So many web frameworks spend so much time reimplementing what
  the browser already has with links, pages, history, and so on.
  We're also avoiding any CSS framework and trying to keep the
  layout as basic as possible.

- Simple file structure and URLs.

  I like short and clean links I can remember.
  The use of folders also makes it easy to have a hierarchical
  structure if we need that in future.

- Minimal management overhead.

  It's nice to write and put thoughts into words
  (even if sometimes it's very hard).
  The site generator should work in an obvious way and not add
  much additional overhead or work.

Unsurprisingly, these mirror our thoughts on software and web development as a whole.
That is, pessimism and disappointment at the state of affairs, and
a yearning for _something better_ as we believe it can and should be.

These goals are enough for now.
I don't think the last two will change, but in time
I might look at fancier styling and client-side effects.
Right now, I'm quite happy having this place as it is.

Unlike other things we've made, this does feel like something of our own.
