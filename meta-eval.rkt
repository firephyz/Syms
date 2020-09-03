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
(define meta-namespace (variable-reference->namespace (#%variable-reference replace-atoms)))
;; Must replace macro keywords with hygenic symbols for this expansion.
;; Avoids sub-macro actions so we don't accidentally replace keywords for that sub-macro
(define (make-hygenic ident-map action)
  ;; Maps a function to sub-macro actions. Used here to make opaque or visable the macro action.
  (define (map-sub-macros func code)
    (if (pair? code)
        (if (eq? 'macro (car code))
            (append (take code 3)
                    (list (func (car (list-tail code 3)))))
            (cons (map-sub-macros func (car code))
                  (map-sub-macros func (cdr code))))
        code))
  (let* ((code-ast (map-sub-macros ;; make sub-macros opaque
                    (lambda (macro) (lambda () macro))
                    action))
         (hygenic-ast (replace-atoms ident-map code-ast)) ;; replace macro keywords
         (code-ast-expanded (map-sub-macros ;; make sub-macros visable again
                             (lambda (macro) (macro))
                             hygenic-ast)))
    code-ast-expanded))

;; macro for builtin functions
;; adjust final bind-idents unquote to use meta-macro binding rather than expanded binding
(define-syntax-rule (builtin form native-action)
  (macro form '() (let* ((builtin-native (lambda (cdr 'form) native-action))
                          (builtin-procedure (lambda (benv)
                                               (let ((values (evlis *args* benv)))
                                                 (apply builtin-native values))))
                          (calling-closure `(lambda ,*args* ,builtin-procedure)))
                     (meval `(,calling-closure ,@*binds*) *denv*))))

; macro variable expansions, quote and unquote or implicitly based on meta-level of binding?
; meta-level as part of type?
; swap builtin and macro for builtin being defined first and macro second?
; make sure bindings introduced in macro expansion and in evaluation are hygenic.
; difference between unquote and eval?

; macro for special forms
(define-syntax-rule (macro form enclosing-env action)
  ;; Generates hygenic identifiers to replace macro keywords in the macro expansion.
  ;; Returns a 'macro-object' for use in meval.
  (let* ((menv-ident (gensym))
         (denv-ident (gensym))
         (args-ident (gensym))
         (binds-ident (gensym))
         (hygenic-ident-map `((*menv* ,menv-ident)
                              (*denv* ,denv-ident)
                              (*args* ,args-ident)
                              (*binds* ,binds-ident)))
         (hygenic-action (make-hygenic hygenic-ident-map 'action))
         (action-callback `(lambda ,(cdr 'form) ,hygenic-action))
         (resolved-enclosing-env enclosing-env)
         (macro-code-ast `(lambda (,menv-ident)
                            (lambda (,denv-ident)
                              (let* ((,args-ident ',(cdr 'form))
                                     (,binds-ident (evlis ,args-ident ,menv-ident)))
                                (begin (printf "EXPAND: ") (pretty-print ',action-callback) (apply ,action-callback ,binds-ident)))))))
    `(,(begin ;(pretty-print macro-code-ast)
              (eval macro-code-ast meta-namespace))
      ,(cdr 'form)
      ,resolved-enclosing-env)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval and special form & function defs below
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; when to eval macro results? During expansion (immediately after) or after expansion in meval
(define (meval expr env)
  (if (procedure? expr)
      (expr env)
      (if (atom? expr)
          (begin (printf "LOOKUP: ")
                 (pretty-print expr)
                 (lookup expr env))
          (let* ((macro (meval (car expr) env))
                 (macro-expander (car macro))
                 (macro-args (cadr macro))
                 (macro-lenv (caddr macro))
                 (macro-env (extend-env macro-lenv (make-frame macro-args (cdr expr)))))
            (meval (meval macro-expander macro-env) env)))))

(define macro-def
  (macro (macro form body) '()
         (macro (() expr) *denv*
                (meval expr (extend-env (cdr *menv*)
                                        (make-frame `(,(car form))
                                                    (macro (() ,@(cdr form)) *denv*
                                                           (meval body (extend-env (cdr *menv*) (make-frame (cdr form) *binds*))))))))))

;; create anonymous macro that takes the closure arguments, evaluates them, extends the lexical environment and calls eval on the body
;; should this macro use a meta-macro in its expansion? Reason for not: ...
;; closure macro environment will contain closure-macro bindings for arguments that are then shadowed
;; by the actual argument value bindings in the dynamic environment once the closure is called. Those
;; macro arguments are no longer needed so toss them away.
(define lambda-def
  (macro (lambda vars body) '()
         ;(let* ((macro-body `(macro (() ,@vars) ,*denv* (meval ',body (extend-env (cdr *menv*) (make-frame ',vars (evlis *binds* *denv*)))))))
         (let ((mform `(() ,@vars)))
           ;(eval macro-body meta-namespace))))
           (macro `(() ,@vars) *denv* (meval body (extend-env (cdr *menv*) (make-frame vars (evlis *binds* *denv*))))))))

;; can this be implemented once macro is defined?
(define if-def
  (macro (if cond then else) '()
         (if (eq? (meval cond *denv*) 'T)
             (meval then *denv*)
             (meval else *denv*))))

(define quote-def
  (macro (quote expr) '() expr))

(define cons-def  (builtin (cons c0 c1) (cons c0 c1)))
(define car-def   (builtin (car c0)     (caar c0)))
(define cdr-def   (builtin (cdr c0)     (cdar c0)))
(define eq?-def   (builtin (eq? s0 s1)  (if (eq? s0 s1) 'T 'F)))
(define atom?-def (builtin (atom? s0)   (if (not (pair? s0)) 'T 'F)))


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

(define builtins `((macro ,macro-def) (cons ,cons-def) (car ,car-def) (cdr ,cdr-def) (eq? ,eq?-def) (atom? ,atom?-def) (if ,if-def) (lambda ,lambda-def) (quote ,quote-def)))
;(define builtins `((lambda ,lambda-def) (quote ,quote-def)))
(define defs `())
(define global `(,defs ,builtins))

;(meval '((lambda (x) (if (atom? (quote (b))) x (cons x x))) (quote a))
;       global)

;(meval '((lambda (x y) (if (atom? x) (if (eq? x y) x (cons x y)) (cons x y))) (cons (quote a) (quote a)) (quote b)) global)
(meval '((lambda (x y) (cons x y)) (quote a) (quote b)) global)
;(meval '((macro (test x) (cons x x)) (test a)) global)
;(meval '(quote a) global)