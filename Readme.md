Lisp implementation with minimal abstract machine. Basically a modified SECD machine with only
symbols, cons cells and "native procedures" (no constants, numbers, functionsor special forms in eval). Everything including special forms and function calls are accomplished with symbol manipulation alone. See meta-eval.rkt.
