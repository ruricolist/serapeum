A possibly over-engineered `range` function. Why is it worth all the
fuss? It's used extensively in Serapeum's test suite. The faster
`range` runs, and the less pressure it puts on the garbage collector,
the faster the test suite runs.

[range]: https://docs.python.org/2/library/functions.html#range
