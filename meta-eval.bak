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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eval and special form & function defs below

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

;; can this macro frame be safely replaced by the evaled frame made below? The new frame show perfectly shadow the macro frame.
(define lambda-def
  `(,(lambda (macro-env)
       (lambda (closure-env)
         (let ((vars (meval 'vars macro-env))
               (body (meval 'body macro-env)))
           `(,(lambda (lex-env)
                (lambda (dyn-env)
                  (let ((bindings (evlis vars lex-env)))
                    (meval body (extend-env (cdr lex-env) (make-frame vars (evlis bindings dyn-env)))))))
             ,vars
             ,closure-env))))
    (vars body)
    ()))
      
(define cons-def
  (let ((arg-names '(c0 c1)))
    `(,(lambda (macro-env)
         (let* ((bindings (evlis arg-names macro-env))
                (builtin (lambda (env)
                           (let ((values (evlis bindings env)))
                             (cons (car values) (cadr values))))))
           `((lambda ,arg-names ,builtin) ,@bindings)))
      ,arg-names
      ())))
;
;
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
(define quote-def
  (let ((arg-names '(expr)))
    `(,(lambda (macro-env)
         (lambda (dyn-env)
           (let ((bindings (evlis arg-names macro-env)))
             (car bindings))))
      ,arg-names
      '())))

;(macro (cons x-expr y-expr)
;  ((lambda (x y)
;     (cons-builtin x y)) x-expr y-expr))
        
;;; quote
;(lambda (expr env)
;  (car expr))

;(define builtins `((cons ,cons) (car ,car) (cdr ,cdr) (eq? ,eq?) (atom? ,(lambda (x) (not (pair? x))))))
(define builtins `((cons ,cons-def) (lambda ,lambda-def) (quote ,quote-def)))
;(define defs `((macro ,macro-def) (lambda ,lambda-def) (num 5) (cons ,(make-cons-def builtins))))
(define defs `((a 1) (b 2)))
(define global `(,defs ,builtins))

; (meval '((macro (quote thing) thing) ((lambda (x) x) (quote hello))) global)
; (meval '((lambda (x) x) num) global)
(meval '((lambda (x y) (cons x y)) a (quote b)) global)
; (macro-bind '((x y) z) '((hi bye) hello))