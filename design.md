# gen
We will design a very simple static site generator to do exactly what we need and no more. This will tentatively be called **gen**.

## Main goals

- Compile Markdown to HTML with pandoc.
  - With syntax highlighting for code.
  - With KaTeX for displaying maths.
- Generate pages which list other pages.
- Basic templating.
- Obtain date from file name or otherwise.

### Details

- Type annotations throughout, as much as possible.
- Class-based interface with implicit context and procedural mutation of the context.
- Matcher classes for matching paths by glob, file extension, and others.
- Files will be represented as a class storing its input path, output path, data, format, and other metadata.
- Transformer classes which can be applied to files such as Pandoc.

This interface is meant to somewhat resemble Hakyll.

## Secondary goals

- Table of contents extracted from Pandoc output.

## Stretch goals

- Paginated list pages.
- General recursive templates.

## Non goals

- Compile speed.
- Configuration file.
- Manual file parsing.
