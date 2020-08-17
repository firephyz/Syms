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

; can this macro frame be safely replaced by the evaled frame made below?
; The new frame show perfectly shadow the macro frame.
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

;; can this be implemented once macro is defined?
(define if-def
  (let ((arg-names '(cond then else)))
    `(,(lambda (macro-env)
         (lambda (dyn-env)
           (let ((bindings (evlis arg-names macro-env)))
             (if (eq? (meval (car bindings) dyn-env) 'T)
                 (meval (cadr bindings) dyn-env)
                 (meval (caddr bindings) dyn-env)))))
      ,arg-names
      ())))

(define quote-def
  (let ((arg-names '(expr)))
    `(,(lambda (macro-env)
         (lambda (dyn-env)
           (let ((bindings (evlis arg-names macro-env)))
             (car bindings))))
      ,arg-names
      '())))
  
(define-syntax-rule (builtin form native-action)
  (let ((func (car form))
        (args (cdr form)))
    (let ((arg-names args))
      `(,(lambda (macro-env)
           (let* ((bindings (evlis arg-names macro-env))
                  (builtin (lambda (env)
                             (let ((values (evlis arg-names env)))
                               (native-action values)))))
             `((lambda ,arg-names ,builtin) ,@bindings)))
        ,arg-names
        ()))))

(define cons-def (builtin '(cons c0 c1) (lambda (values) (cons (car values) (cadr values)))))
(define car-def (builtin '(car c0) (lambda (values) (car (car values)))))
(define cdr-def (builtin '(cdr c0) (lambda (values) (cdr (car values)))))
(define eq?-def (builtin '(eq? s0 s1) (lambda (values) (if (eq? (car values) (cadr values)) 'T 'F))))
(define atom?-def (builtin '(atom? s0) (lambda (values) (if (not (pair? (car values))) 'T 'F))))


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

(define builtins `((cons ,cons-def) (car ,car-def) (cdr ,cdr-def) (eq? ,eq?-def) (atom? ,atom?-def) (if ,if-def) (lambda ,lambda-def) (quote ,quote-def)))
(define defs `((a one) (b two)))
(define global `(,defs ,builtins))

(meval '((lambda (x) (atom? x)) (quote a))
       global)

;(meval '((lambda (x) x) (quote a)) global)