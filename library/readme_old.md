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
Compiler can utilize source location to automatically propagate refactorizations.

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

(has is (function which ))
(from is (function which
           (has property infix)))

(collection
  (has ))

abstract
(function is abstract
  (use lib from library (syntax function))
  (has signature defaults )
  (has property match-method from lib)
    defaults ))

(is is ((function which
          (has signature (word statement))
          (has property infix from (library (syntax function match-methods)))) which is
  (procedure (library store word statement))))

(library is (collection of definition)    (comment what is a thing though?)
  (has (function search path item) which is
    (procedure (comment (use (find from (library verbs))))
               (box is (get path))
               (definition from box)))
  (has (function store word statement) which is
    ())
  (has property callable search))

(number is (list of digits)
  (has (add y) from (library (arithmetic operations))

(digits
  (0)
  (1 suc(0))
  (2 suc(1))
  (9 suc(8))
(add x y)
  (if (= (size ) 1)
      ((get x -1)
