#lang eopl

(require racket/list)
; ------------------------------------------------------------------------------
; Environments

; See: exercise 2.5 and 2.10

; Approximation.
(define environment? list?)

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

(define extend-env*
  (lambda (vars vals env)
    (cond ((and (null? vars) (null? vals))
           env)
          ((and (pair? vars) (pair? vals))
           (extend-env* (cdr vars)
                        (cdr vals)
                        (extend-env (car vars) (car vals) env)))
          ((null? vars)
           (report-too-few-variables))
          (else
           (report-too-few-values)))))

(define report-too-few-variables
  (lambda ()
    (eopl:error 'extend-env* "Too few variables")))

(define report-too-few-values
  (lambda ()
    (eopl:error 'extend-env* "Too few values")))

(define apply-env
  (lambda (initial-env search-var)
    (letrec ((loop (lambda (env)
                     (cond ((null? env)
                            (report-no-binding-found search-var initial-env))
                           ((and (pair? env) (pair? (car env)))
                            (let ((saved-var (caar env))
                                  (saved-val (cdar env))
                                  (saved-env (cdr env)))
                              (if (eqv? search-var saved-var)
                                  saved-val
                                  (loop saved-env))))
                           (else
                            (report-invalid-env initial-env))))))
      (loop initial-env))))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))

; ------------------------------------------------------------------------------
; Exercise 3.19

; See: exercise 3.18
; Note that we do not *replace* proc by letproc, but add the latter.
;
; Expression ::= letproc Identifier (Identifier) = Expression in Expression
;                (letproc-exp (name var proc-body exp-body)
;
; (value-of (letproc-exp name var proc-body exp-body) rho)
; = (value-of exp-body [name=(proc-val (procedure var proc-body rho))] rho)

; ------------------------------------------------------------------------------
; Scanner and parser specification

(define scanner-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (nullary-op ("emptylist") string)
    (unary-op ((or "zero?" "minus" "car" "cdr" "null?" "print")) string)
    (binary-op ((or "+" "-" "*" "/" "equal?" "greater?" "less?" "cons")) string)
    (n-ary-op ("list") string)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)))

; Approximation.
(define identifier? symbol?)

(define grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (nullary-op) nullary-exp)
    (expression (unary-op "(" expression ")") unary-exp)
    (expression (binary-op "(" expression "," expression ")") binary-exp)
    (expression (n-ary-op "(" (separated-list expression ",") ")") n-ary-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("let*"(arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("traceproc" "(" (arbno identifier) ")" expression) traceproc-exp)
    (expression ("letproc" identifier "(" (arbno identifier) ")" "=" expression "in" expression) letproc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)

; ------------------------------------------------------------------------------
; Debugging helpers for scanner and parser

(define list-the-datatypes
  (lambda()
    (sllgen:list-define-datatypes scanner-spec grammar)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; ------------------------------------------------------------------------------
; Representing procedures as data structures

;(define-datatype proc proc?
;  (procedure
;   (var (list-of identifier?))
;   (body expression?)
;   (saved-env? environment?)
;   (trace? boolean?)))
;
;(define apply-procedure
;  (lambda (proc1 vals)
;    (cases proc proc1
;      (procedure (vars body saved-env trace?)
;                 (let ((value (value-of body (extend-env* vars vals saved-env))))
;                   (if trace?
;                       (begin
;                         (eopl:printf "enter procedure: ~a = ~a\n" vars vals)
;                         (eopl:printf "~a\nexist procedure.\n" value)
;                         value)
;                       value))))))

;; procedure without environment and corresponding apply-procedure 
(define-datatype proc proc?
  (procedure
   (var (list-of identifier?))
   (body expression?)
   ;(saved-env? environment?)
   (trace? boolean?)))

(define apply-procedure
  (lambda (proc1 vals env)
    (cases proc proc1
      (procedure (vars body trace?)
                 (let ((value (value-of body (extend-env* vars vals env))))
                   (if trace?
                       (begin
                         (eopl:printf "enter procedure: ~a = ~a\n" vars vals)
                         (eopl:printf "~a\nexist procedure.\n" value)
                         value)
                       value))))))

; ------------------------------------------------------------------------------
; Expressed values

; We directly embed the two list constructors here, as an alternative we could
; have a single list-val wrapping a pair or the empty list. The version below
; is closer to a real implementation and avoids some ping-pong between types.
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (emptylist-val)
  (cons-val (first expval?) (rest expval?))
  (proc-val (proc proc?)))

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))

(define expval->car
  (lambda (val)
    (cases expval val
      (cons-val (first rest) first)
      (else (report-expval-extractor-error 'cons val)))))

(define expval->cdr
  (lambda (val)
    (cases expval val
      (cons-val (first rest) rest)
      (else (report-expval-extractor-error 'cons val)))))

(define expval->emptylist?
  (lambda (val)
    (cases expval val
      (emptylist-val () #t)
      (cons-val (first rest) #f)
      (else (report-expval-extractor-error 'cons-or-emptylist val)))))

; No 'foldr' in #lang eopl, use a specialized version for 1 list.
(define foldr-1
  (lambda (func init lst)
    (if (null? lst)
        init
        (func (car lst) (foldr-1 func init (cdr lst))))))

(define list-val
  (lambda (elements)
    (foldr-1 cons-val (emptylist-val) elements)))

(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc1) proc1)
      (else (report-expval-extractor-error 'procedure val)))))

(define report-expval-extractor-error
  (lambda (expected val)
    (eopl:error 'expval-extractor "Expected a ~s, got ~s" expected val)))

; ------------------------------------------------------------------------------
; A printer for expressed values.

(define print
  (lambda (val)
    (display (expval->sexp val))
    (newline)
    (num-val 1)))

(define expval->sexp
  (lambda (val)
    (cases expval val
      (num-val (num)
               num)
      (bool-val (bool)
                bool)
      (emptylist-val ()
                     '())
      (cons-val (first rest)
                (cons (expval->sexp first) (expval->sexp rest)))
      (proc-val (proc1)
                "<<procedure>>"))))

; ------------------------------------------------------------------------------
; Init environment
(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
                (extend-env 'v (num-val 5)
                            (extend-env 'x (num-val 10)
                                        (empty-env))))))

; ------------------------------------------------------------------------------
; Interpreter

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (nullary-exp (op)
                   ((nullary-table op)))
      (unary-exp (op exp1)
                 (let ((val1 (value-of exp1 env)))
                   ((unary-table op) val1)))
      (binary-exp (op exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    ((binary-table op) val1 val2)))
      (n-ary-exp (op exprs)
                 (let ((vals (map (lambda (expr) (value-of expr env)) exprs)))
                   ((n-ary-table op) vals)))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (cond-exp (lhss rhss)
                (value-of-cond lhss rhss env))
      (var-exp (var)
               (apply-env env var))
      (let-exp (var exp1 body)
               (let ((val1 (value-of exp1 env)))
                 (value-of body (extend-env var val1 env))))
      (let*-exp (vars exprs body)
                (value-of body (foldl-2 extend-with-value-of env vars exprs)))
      (unpack-exp (vars expr body)
                  (let ((val (value-of expr env)))
                    (value-of body (extend-env* vars (expval->list val) env))))
      (proc-exp (vars body)
                (let ((frees (free-variables body '())))
                  (proc-val (procedure vars body #f))))
      (traceproc-exp (vars body)
                     (let ((frees (free-variables body '())))
                       (proc-val (procedure vars body #t))))
      (letproc-exp (name vars proc-body exp-body)
                   (let ((p-val (proc-val (procedure vars proc-body #f))))
                     (value-of exp-body (extend-env name p-val env))))
      (call-exp (rator rands)
                (let ((proc (expval->proc (value-of rator env)))
                      (args (value-of* rands env)))
                  (apply-procedure proc args env))))))

(define free-variables
  (lambda (expr bound)
    (cases expression expr
      (const-exp (num) '())
      (nullary-exp (op) '())
      (unary-exp (op exp1)
                 (free-variables exp1 bound))
      (binary-exp (op exp1 exp2)
                  (append (free-variables exp1 bound)
                          (free-variables exp2 bound)))
      (n-ary-exp (op exprs)
                 (append (flatten (map free-variables exprs))))
      (var-exp (var)
               (if (memq var bound)
                   '()
                   (list var)))
      (if-exp (pred consq alte)
              (append (free-variables pred bound)
                      (free-variables consq bound)
                      (free-variables alte bound)))
      (cond-exp (lhss rhss)
                (append (free-variables lhss bound)
                        (free-variables rhss bound)))
      (let-exp (var value body)
               (append (free-variables value bound)
                       (free-variables body (cons var bound))))
      (let*-exp (vars exprs body)
                (flatten (map (lambda (var)
                                (free-variables var bound))
                              vars)))
      (unpack-exp (vars expr body)
                  (flatten (map (lambda (var)
                                  (free-variables var bound))
                                vars)))
      (proc-exp (var body)
                (append (free-variables body (cons var bound))))
      (traceproc-exp (var body)
                     (append (free-variables body (cons var bound))))
      (letproc-exp (name vars proc-body exp-body)
                   (flatten (map (lambda (var)
                                   (free-variables var bound))
                                 vars)))
      (call-exp (rator rands)
                (append (free-variables rator bound)
                        (flatten (map (lambda (var)
                                        (free-variables var bound))
                                      rands)))))))

(define optimize-env
  (lambda (env frees)
    (let ((pred (lambda(var)
                  (if (or (null? var)
                          (memq (car var) frees))
                      #t
                      #f))))
      (filter pred env))))

(define filter
  (lambda (pred lst)
    (cond
      ((null? lst) '())
      ((pred (car lst)) (cons (car lst)
                              (filter pred (cdr lst))))
      (else (filter pred (cdr lst))))))
                      
(define value-of*
  (lambda (exps env)
    (letrec ((value-of*-helper (lambda (exps env init)
                                 (cond
                                   ((null? exps) init)
                                   (else
                                    (value-of*-helper (cdr exps)
                                                      env
                                                      (cons (value-of (car exps) env)
                                                            init)))))))
      (value-of*-helper exps env empty))))

(define value-of-cond
  (lambda (lhss rhss env)
    (cond ((null? lhss)
           (eopl:error 'value-of-cond "No LHS evaluated to true"))
          ((expval->bool (value-of (car lhss) env))
           (value-of (car rhss) env))
          (else
           (value-of-cond (cdr lhss) (cdr rhss) env)))))

; No 'foldl' in #lang eopl, use a specialized version for 2 lists.
(define foldl-2
  (lambda (func init lst1 lst2)
    (if (null? lst1)
        init
        (foldl-2 func
                 (func (car lst1) (car lst2) init)
                 (cdr lst1)
                 (cdr lst2)))))

(define extend-with-value-of
  (lambda (var expr env)
    (extend-env var (value-of expr env) env)))

(define expval->list
  (lambda (val)
    (cases expval val
      (emptylist-val () '())
      (cons-val (first rest) (cons first (expval->list rest)))
      (else (report-expval-extractor-error 'cons-or-emptylist val)))))

; ------------------------------------------------------------------------------
; A table for n-ary operations, represented as an association list.

(define make-table
  (lambda lst
    (lambda (op)
      (cdr (assoc op lst)))))

(define entry cons)

; ------------------------------------------------------------------------------
; Various lifting functions.

(define unary-arithmetic-predicate
  (lambda (func)
    (lambda (val)
      (bool-val (func (expval->num val))))))

(define unary-arithmetic-func
  (lambda (func)
    (lambda (val)
      (num-val (func (expval->num val))))))

(define unary-list-predicate
  (lambda (func)
    (lambda (val)
      (bool-val (func val)))))

(define binary-relation
  (lambda (func)
    (lambda (val1 val2)
      (bool-val (func (expval->num val1) (expval->num val2))))))

(define binary-arithmetic-func
  (lambda (func)
    (lambda (val1 val2)
      (num-val (func (expval->num val1) (expval->num val2))))))

; ------------------------------------------------------------------------------
; Tables of n-ary operations.

(define nullary-table
  (make-table (entry "emptylist" emptylist-val)))
  
(define unary-table
  (make-table (entry "zero?" (unary-arithmetic-predicate zero?))
              (entry "minus" (unary-arithmetic-func -))
              (entry "car" expval->car)
              (entry "cdr" expval->cdr)
              (entry "null?" (unary-list-predicate expval->emptylist?))
              (entry "print" print)))

(define binary-table
  (make-table (entry "+" (binary-arithmetic-func +))
              (entry "-" (binary-arithmetic-func -))
              (entry "*" (binary-arithmetic-func *))
              (entry "/" (binary-arithmetic-func quotient))
              (entry "equal?" (binary-relation =))
              (entry "greater?" (binary-relation >))
              (entry "less?" (binary-relation <))
              (entry "cons" cons-val)))

(define n-ary-table
  (make-table (entry "list" list-val)))

; ------------------------------------------------------------------------------
; A nice REPL for interactive use

(define read-eval-print
  (sllgen:make-rep-loop "-->" value-of-program
                        (sllgen:make-stream-parser scanner-spec grammar)))

;; test
;(run "let makemult = proc (maker)
;                      proc (x)
;                       if zero?(x)
;                       then 0
;                       else -(((maker maker) -(x,1)), -4)
;      in let times4 = proc (x) ((makemult makemult) x)
;         in (times4 3)")
;
;(run "let makemult = proc (maker)
;                      proc (x)
;                       if zero?(x)
;                       then 0
;                       else +(((maker maker) -(x,1)), 4)
;      in let times4 = proc (x) ((makemult makemult) x)
;         in (times4 3)")
;
;(run "let makemult = proc (maker)
;                      proc (x)
;                       if zero?(x)
;                       then 0
;                       else -(((maker maker) -(x,1)), minus(x))
;      in let times4 = proc (x) ((makemult makemult) x)
;         in (times4 4)")

;(run "let makemult = proc (maker)
;                      proc (y)
;                       proc (x)
;                        if zero?(x)
;                        then 0
;                        else +((((maker maker) y) -(x,1)), y)
;      in let times = proc (x y) (((makemult makemult) y) x)
;         in let frac = proc (f)
;                        proc (n)
;                         if zero?(n)
;                         then 1
;                         else (times n ((f f) -(n,1)))
;              in ((frac frac) 4)")

;(run "let odd-iter = proc (od)
;                      proc (ev)
;                       proc (x)
;                        if zero?(x)
;                        then 0
;                        else (((ev ev) od) -(x,1))
;      in let even-iter = proc (ev)
;                          proc (od)
;                           proc (x)
;                            if zero?(x)
;                            then 1
;                            else (((od od) ev) -(x,1))
;         in let odd = proc (n) (((odd-iter odd-iter) even-iter) n)
;           in let even = proc (n) (((even-iter even-iter) odd-iter) n)
;             in (odd 3)")

;(run "let makerec = proc (f)
;                     let d = proc (x)
;                              proc (z) ((f (x x)) z)
;                     in proc (n) ((f (d d)) n)
;        in let maketimes4 = proc (f) proc (x)
;                            if zero?(x)
;                            then 0
;                            else -((f -(x,1)), -4)
;          in let times4 = (makerec maketimes4) in (times4 3)")

;(run "(proc (x y) +(x, y)
;       3 4)")
;
;(run "let f = proc (x) proc (y) +(y, +(x, 1))
;      in ((f 3) 4)")
;
;(run "let f = proc (x) -(x,11)
;      in (f (f 77))")
;(run "let f = traceproc (x) -(x,11)
;      in (f 77)")
;
;(run "let f = traceproc (x) -(x,11)
;      in (f (f 77))")
;
;(run "(proc (f) (f (f 77))
;       proc (x) -(x,11))")

(run "let a = 3
      in let* p = proc (x) -(x,a)
             a = 5 
         in -(a,(p 2))")

;; test from exercise 3.37
(run "let fact = proc (n) add1(n) in let fact = proc (n)
                          if zero?(n)
                          then 1
                          else *(n,(fact -(n,1)))
       in (fact 5)")
