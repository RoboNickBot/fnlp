# FNLP v0.0.0.0 :: purely functional natural language processing

`fnlp` is a library for building NLP applications and experiments.
It's sort of modeled on [NLTK](http://www.nltk.org/).  Right now,
everything is very much **under construction**.

## Quick start

There aren't any quickly accessible modules or tools yet.  More
documentation needs writing.  You can look at the `Database.FNLP`
module for some canned writing-system classification operations.

## Features

Right now, the library can only perform writing-system classification
using cosines of trigram frequency lists.  For this purpose, it has a
database model (sqlite3) for storing text corpora and functions for
properly streaming (using
[`Pipes`](http://hackage.haskell.org/package/pipes)) the operations.

## Philosophy

I'd like to keep this library limited to logic and algorithms, and
push real-world concerns (like efficient storage of language data) to
something like `fnlp-io` or `fnlp-database`.  For now though, it's all
here.

Actual executables should be their own (hopefully small and concise,
if this library is doing its job) projects.  A generally useful
collection of such things could go in something like `fnlp-tools`.