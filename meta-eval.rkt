#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-frame vars args)
  (define (helper vars args result)
    (if (null? vars)
        result
        (helper (cdr vars) (cdr args) (cons (list (car vars) (car args)) result))))
  (helper vars args '()))

(define (macro-bind form code)
  (if (null? form)
      '()
      (if (pair? (car form))
          (append (macro-bind (car form) (car code)) (macro-bind (cdr form) (cdr code)))
          (cons (list (car form) (car code)) (macro-bind (cdr form) (cdr code))))))

(define (evlis args env)
  (if (null? args)
      '()
      (cons (meval (car args) env) (evlis (cdr args) env))))

(define (extend-env env frame)
  (cons frame env))

(define (lookup sym env)
  (define (frame-lookup frame)
    (if (null? frame)
        '()
        (if (eq? (caar frame) sym)
            (cadar frame)
            (frame-lookup (cdr frame)))))
  (if (null? env)
      '()
      (let ((frame-result (frame-lookup (car env))))
        (if (null? frame-result)
            (lookup sym (cdr env))
            frame-result))))

(define (atom? expr) (not (pair? expr)))

;; match on predicates? null? atom? etc.
(define (replace-atoms replace-pairs item)
  (define (replace-atom-single target value item)
    (if (or (atom? item) (null? item))
        (if (eq? item target)
            value
            item)
        (cons (replace-atom-single target value (car item))
              (replace-atom-single target value (cdr item)))))
  (if (null? replace-pairs)
      item
      (replace-atoms (cdr replace-pairs)
                     (replace-atom-single (caar replace-pairs) (cadar replace-pairs) item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (builtin form native-action)
  (let ((func (car form))
        (args (cdr form)))
    `(,(lambda (macro-env)
         (let* ((bindings (evlis args macro-env))
                (builtin (lambda (env)
                           (let ((values (evlis args env)))
                             (native-action values)))))
           `((lambda ,args ,builtin) ,@bindings)))
      ,args
      ())))

; macro variable expansions, quote and unquote or implicitly based on meta-level of binding?
; meta-level as part of type?
(define test '())
(define-syntax-rule (macro form enclosing-env action)
  (let* ((menv-ident (string->symbol "macro-env-0"))
         (denv-ident (string->symbol "dyn-env-0"))
         (binds-ident (string->symbol "macro-binds-0"))
         (hygenic-action (replace-atoms `((macro-env ,menv-ident)
                                          (dyn-env ,denv-ident)
                                          (macro-bindings ,binds-ident))
                                        action))
         (macro-code-ast `(lambda (,menv-ident)
                            (lambda (,denv-ident)
                              (let ((,binds-ident (evlis (cdr form) ,menv-ident)))
                                ,hygenic-action)))))
    (begin (set! test (namespace-mapped-symbols (variable-reference->namespace (#%variable-reference menv-ident))))
           `(,(eval macro-code-ast (empty-namespace))
             ,(cdr form)
             ,enclosing-env))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval and special form & function defs below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; when to eval macro results? During expansion (immediately after) or after expansion in meval
(define (meval expr env)
  (if (procedure? expr)
      (expr env)
      (if (atom? expr)
          (lookup expr env)
          (let* ((macro (meval (car expr) env))
                 (macro-expander (car macro))
                 (macro-args (cadr macro))
                 (macro-lenv (caddr macro))
                 (macro-env (extend-env macro-lenv (make-frame macro-args (cdr expr)))))
            (meval (meval macro-expander macro-env) env)))))

;(define lambda-def
;  (macro '(lambda vars body) '()
;         (macro `(closure ,@(car macro-bindings)) closure-env
;                  (lambda (lex-env dyn-env closure-bindings)
;                    (meval (cadr macro-bindings)
;                           (extend-env lex-env (make-frame (car macro-bindings)
;                                                           (evlis closure-bindings dyn-env)))))))))

;; can this be implemented once macro is defined?
;(define if-def
;  (macro '(if cond then else) '()
;         (lambda (macro-env dyn-env macro-bindings)
;           (if (eq? (meval (car macro-bindings) dyn-env) 'T)
;               (meval (cadr macro-bindings) dyn-env)
;               (meval (caddr macro-bindings) dyn-env)))))

(define quote-def
  (macro '(quote expr) '()
         '(car macro-bindings)))

;(define cons-def  (builtin '(cons c0 c1) (lambda (values) (cons (car values) (cadr values)))))
;(define car-def   (builtin '(car c0)     (lambda (values) (car (car values)))))
;(define cdr-def   (builtin '(cdr c0)     (lambda (values) (cdr (car values)))))
;(define eq?-def   (builtin '(eq? s0 s1)  (lambda (values) (if (eq? (car values) (cadr values)) 'T 'F))))
;(define atom?-def (builtin '(atom? s0)   (lambda (values) (if (not (pair? (car values))) 'T 'F))))


;;; lambda, eval in first, lexical environments. eval in second, dynamic environments
;;; ambiguity in which environments the lex-env and dyn-env variables refer to?
;(define lambda-def
;  (lambda (expr lex-env)
;    (let ((vars (car expr))
;          (body (cadr expr)))
;      (lambda (expr dyn-env)
;        (let* ((args (evlis expr dyn-env))
;               (frame (make-frame vars args))
;               (new-env (extend-env lex-env frame)))
;          (meval body new-env))))))
;
;;; The environments are meta-closures, is this okay? Can it still be imlemented without builtin support for them?
;;; makes a closure. Can the body be (meval body new-env) instead of body with a more complicated environment?
;(define make-lambda-def
;  (let ((args '(args body)))
;    `((body
;       ,(lambda (lex-env lambda-args) `(cons (make-frame args (lambda evlis ,lambda-args ,lex-env)) ,lex-env)) ;; dynamically construct environment at runtime
;      ,(lambda (macro-env macro-args) (extend-env macro-env (make-frame args macro-args)))))))
;
;

;(define builtins `((cons ,cons-def) (car ,car-def) (cdr ,cdr-def) (eq? ,eq?-def) (atom? ,atom?-def) (if ,if-def) (lambda ,lambda-def) (quote ,quote-def)))
(define builtins `((quote ,quote-def)))
(define defs `((a one) (b two)))
(define global `(,defs ,builtins))

;(meval '((lambda (x) (if (atom? (quote (b))) x (cons x x))) (quote a))
;       global)

(meval '(quote hi) global)