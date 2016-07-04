# elm-cassowaryjs
This library wraps [Cassowary JS](https://github.com/slightlyoff/cassowary.js/), a Javascript implementation of the [Cassowary constraint solver](https://constraints.cs.washington.edu/solvers/cassowary-tochi.pdf). As with any JS wrapper, using this library defeats the point of Elm! Goodbye immutability guarantees.

So why did I write this? I was wondering how a high-level Cassowary API might look in a functional language - turns out it looks pretty great (see the examples). The next step would be to implement Cassowary (basically [Simplex](https://en.wikipedia.org/wiki/Simplex_algorithm) with some modifications) in Elm. I made a start on this and quickly realised I'm more interested in using Cassowary than I am in the implementation. I would consider returning to it if I can get some help.

Here's a demo of the `quad` example:

![Quad Demo](https://github.com/lebek/elm-cassowaryjs/raw/master/quad_demo.gif)
