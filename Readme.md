Lisp (with an extra addition)
Detach runtime space from the hardware and software running it.
Numbers are the main struggle, needed for indexing and such. Implement numbers
as lists of symbols rather than actual bitfields. Arithmetic operations can then
be coded as operations on the lists.
Functions are also a struggle, how to implement functions as symbolic manipulations
rather than as primitives.

Things without definitions are axioms.

Every statement (atom or grouping of atoms) can be annotated with
  (things which annotate). basically prepositions.

things which annotate include...
is = definition
of = instance relation
has = related member
(has property) = related trait
which = constrain type (how to define this?)

get is function which self evaluates its argument. Argument have fetchable property
from sort of places its first argument into the second argument


(function is

(find is (function ))

(package is (list of ...)    (comment what is a thing though?)
  (has (search path item) which is
         (procedure (comment (use (find from (package search verbs))))
                    (box is (get path))
                    (item from box))))

(number is (list of digits)
  (has (add y) from (package search (arithmetic operations))

(digits
  (0)
  (1 suc(0))
  (2 suc(1))
  (9 suc(8))
(add x y)
  (if (= (size ) 1)
      ((get x -1)
