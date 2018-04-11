#lang eopl
; ------------------------------------------------------------------------------
; exercise 3.33
; the code is same as exercise 3.32
; only some naming refine

; ------------------------------------------------------------------------------
; Environments

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var identifier?)
   (val expval?)
   (env environment?))
  (extend-env*
   (vars (list-of identifier?))
   (vals (list-of expval?))
   (env environment?))
  (extend-env-rec
   (p-name (list-of identifier?))
   (b-vars (list-of (list-of identifier?)))
   (body (list-of expression?))
   (env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
                 (report-no-binding-found search-var env))
      (extend-env (saved-var saved-val saved-env)
                  (if (eqv? saved-var search-var) saved-val
                      (apply-env saved-env search-var)))
      (extend-env* (saved-vars saved-vals saved-env)
                   (letrec ((loop
                             (lambda (vars vals env)
                               (cond
                                 ((null? vars)
                                  (apply-env env search-var))
                                 ((equal? (car vars) search-var)
                                  (car vals))
                                 (else (loop (cdr vars)
                                             (cdr vals)
                                             env))))))
                     (loop saved-vars saved-vals saved-env)))
      (extend-env-rec (p-names b-vars p-bodys saved-env)
                       (let ((result (get-proc search-var
                                              p-names
                                              b-vars
                                              p-bodys)))
                         (if (null? result)
                             (apply-env saved-env search-var)
                             (proc-val (procedure (car result)
                                                     (cadr result)
                                                     env))))))))

(define get-proc
  (lambda (search-name pnames pvars pbodys)
    (cond
      ((null? pnames) '())
      ((eqv? search-name (car pnames))
       (list (car pvars) (car pbodys)))
      (else (get-proc search-name
                      (cdr pnames)
                      (cdr pvars)
                      (cdr pbodys))))))

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
    (expression ("let"(arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression) letrec-exp)
    (expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
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

(define-datatype proc proc?
  (procedure
   (vars (list-of identifier?))
   (body expression?)
   (saved-env? environment?)))

(define apply-procedure
  (lambda (proc1 vals)
    (cases proc proc1
      (procedure (vars body saved-env)
                  (value-of body (extend-env* vars vals saved-env)))))) 
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
      (else (report-expval-extractor-error 'procedure* val)))))

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
                 "<<procedure*>>"))))

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
      (let-exp (vars exprs body)
               (value-of body (foldl-2 extend-with-value-of env vars exprs)))
      (letrec-exp (p-names lb-vars p-bodys letrec-body)
                  (value-of letrec-body (extend-env-rec p-names lb-vars p-bodys env)))
      (unpack-exp (vars expr body)
                  (let ((val (value-of expr env)))
                    (value-of body (extend-env vars (expval->list val) env))))
      (proc-exp (vars body)
                (proc-val (procedure vars body env)))
      (letproc-exp (name vars proc-body exp-body)
                   (let ((p-val (proc-val (procedure vars proc-body env))))
                     (value-of exp-body (extend-env name p-val env))))
      (call-exp (rator rands)
                (let ((proc (expval->proc (value-of rator env)))
                      (args (value-of* rands env)))
                  (apply-procedure proc args))))))

(define value-of*
  (lambda (exps env)
    (letrec ((value-of*-helper
              (lambda (exps env init)
                (cond
                  ((null? exps) init)
                  (else
                   (value-of*-helper (cdr exps)
                                     env
                                     (cons (value-of (car exps) env)
                                           init)))))))
      (reverse (value-of*-helper exps env empty)))))

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

;(run "let f = proc (x) -(x,11)
;      in (f (f 77))")
;
;(run "(proc (f) (f (f 77))
;       proc (x) -(x,11))")
;
;(run "let a = 1
;          b = 3
;      in +(a,b)")
;
;(run "let a = 1
;          b = +(3,a)
;      in +(a,b)")
;
;(run "letproc f(a b) = +(a,b)
;      in (f 2 3)")
;
;;(run "letrec double(x) = if zero?(x)
;;                         then 0
;;                         else -((double -(x,1)), -2)
;;      in (double 6)")
;
;(run "letrec multi(x,n) = if zero?(x)
;                           then 0
;                           else -((multi -(x,1) n), minus(n))
;      in (multi 6 3)")

;(run "letrec time(x, y)
;             = if zero?(x) then 0 else -((time -(x,1)  y), -(0, y))
;      in (time 3 2)")

(run "letrec
          even(x) = if zero?(x) then 1 else (odd -(x,1))
          odd(x) = if zero?(x) then 0 else (even -(x,1))
      in (odd 13)")

(run "letrec
      one(x, y) = if zero?(x) then 1 else (two -(x, 1) y)
      two(x, y) = if zero?(y) then 0 else (one x -(y, 1))
       in (two 5 4)")

(run "letrec
      func(x) =
       if zero?(x) then
          1
      else
         -((func -(x, 1)), -(0, x))
      in (func 10)")
