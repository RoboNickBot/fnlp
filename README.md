# FNLP v0.0.0.0 :: purely functional natural language processing

`fnlp` is a library for building NLP applications and experiments.
It's sort of modeled on [NLTK](http://www.nltk.org/).  Right now,
everything is very much **under construction**.

This is also a continuation of the earlier
[nlp-libs](https://github.com/RoboNickBot/nlp-libs) and
[nlp-tools](https://github.com/RoboNickBot/nlp-tools) packages I made
for the [*An Crúbadán*](http://crubadan.org) project.

## Quick start

Importing the `FNLP.Prelude` module brings all of the library's common
types and functions into scope.  (It's admittedly a little sparse at
the moment!)

More documentation to follow!

## Features

Right now, the library can only perform writing-system classification
using cosines of trigram frequency lists.  For this purpose, it has a
database model (sqlite3) for storing text corpora and functions for
properly streaming (using
[`pipes`](http://hackage.haskell.org/package/pipes)) the operations.

## Philosophy

I'd like to keep this library limited to logic and algorithms, and
push real-world concerns (like efficient storage of language data) to
something like `fnlp-io` or `fnlp-database`.  For now though, it's all
here.

Actual executables should be their own (hopefully small and concise,
if this library is doing its job) projects.  A generally useful
collection of such things could go in something like `fnlp-tools`.
