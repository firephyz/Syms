Lisp (with an extra addition)
Detach runtime space from the hardware and software running it.
Numbers are the main struggle, needed for indexing and such. Implement numbers
as lists of symbols rather than actual bitfields. Arithmetic operations can then
be coded as operations on the lists.
Functions are also a struggle, how to implement functions as symbolic manipulations
rather than as primitives.
