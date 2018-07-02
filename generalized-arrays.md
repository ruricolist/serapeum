Some operations on “generalized arrays.”

Functions on generalized arrays are total: they work on arrays, of course, but also on sequences (which are treated as one-dimensional arrays) and atoms (which are treated as zero-dimensional arrays).

### A note for array programmers

The semantics of generalized arrays in Serapeum is based on the “array theory” formalism of Trenchard More, as implemented in [Nial][]. Note that this is different from the MOA (“Mathematics of Arrays”) formalism on which direct descendants of APL, such as J, are based.

Nial programmers might be surprised that we rely on the v4, rather than the v6, version of array theory. This is because, in Common Lisps, it is possible to have empty arrays of different element types, and such arrays are not considered equivalent.

[Nial]: https://en.wikipedia.org/wiki/Nial
