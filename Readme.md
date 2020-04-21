Lisp (with an extra addition)
Detach runtime space from the hardware and software running it.
Numbers are the main struggle, needed for indexing and such. Implement numbers
as lists of symbols rather than actual bitfields. Arithmetic operations can then
be coded as operations on the lists.
Functions are also a struggle, how to implement functions as symbolic manipulations
rather than as primitives.

Obsolete things can be marked with the archaic property to make the compiler issue a warning.

Compiler to use annotations where necessary to optimize and prove functionality.
Compiler can output adjusted source with derived types and such (what do
  functions return, etc.)

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
from sort of gets its first argument from the second argument


(function is

(collection is (abstract ))
(is is ((function which
          (has property infix))
        [of] word statement) which is
  (procedure (vocab store word statement)))
(vocab is (collection of definition)    (comment what is a thing though?)
  (has (function path item) which is
    (procedure (comment (use (find from (vocab search verbs))))
               (box is (get path))
               (item from box)))
  (has (function store word statement) which is
    ()))

(number is (list of digits)
  (has (add y) from (vocab search (arithmetic operations))

(digits
  (0)
  (1 suc(0))
  (2 suc(1))
  (9 suc(8))
(add x y)
  (if (= (size ) 1)
      ((get x -1)
