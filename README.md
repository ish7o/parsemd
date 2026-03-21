# parsemd

Monadic markdown parser written in Haskell, built from scratch without libraries.

## Bit of history

Coming from Go and Python, Haskell's type system and purely functional approach was a different mental model. I wrote this to gain Haskell experience and learn how to structure a parser using only Functor, Applicative[,](https://en.wikipedia.org/wiki/Serial_comma) and Monad instances.

## What works:
- Headings
- Paragraphs
- Blockqotes
- Horizontal rules
- Bold
- Italic
- Inline code
- Links
- Strikethrough
- Ordered/Unordered lists
- Code blocks

## Usage
Reads stdin
```sh
cabal run
```

## TODO
- [ ] Recursive list parsing
- [ ] Escape characters (e.g. `\*text\*`)
- [ ] HTML output
