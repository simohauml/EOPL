#lang eopl

;; ----------------------------------------------
;; environment

;; Senv = Listof(Sym)
;; Lexaddr = N
;; empty-senv : () → Senv
(define empty-senv
  (lambda ()
    '()))

;; extend-senv : Var × Senv → Senv
(define extend-senv
  (lambda (var senv)
    (cons var senv)))

;; apply-senv : Senv × Var → Lexaddr
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv)
       (report-unbound-var var))
      ((eqv? var (car senv))
       0)
      (else
       (+ 1 (apply-senv (cdr senv) var))))))

(define report-unbound-var
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

;; nameless-environment? : SchemeVal → Bool
(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

;; empty-nameless-env : () → Nameless-env
(define empty-nameless-env
  (lambda ()
    '()))

;; extend-nameless-env : ExpVal × Nameless-env → Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

;; apply-nameless-env : Nameless-env × Lexaddr → ExpVal
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))

;; init-senv : () → Senv
(define init-senv
  (lambda ()
    (extend-senv
     'i
     (extend-senv
      'v
      (extend-senv
       'x
       (empty-senv))))))

(define init-nameless-env
  (lambda ()
    (extend-nameless-env
     (num-val 1)                        ; was i
     (extend-nameless-env
      (num-val 5)                       ; was v
      (extend-nameless-env
       (num-val 10)                     ; was x
       (empty-nameless-env))))))

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
    ;(expression ("let"(arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    ;(expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression) "in" expression) letrec-exp)
    ;(expression ("unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    ;(expression ("letproc" identifier "(" (arbno identifier) ")" "=" expression "in" expression) letproc-exp)
    (expression ("(" expression expression ")") call-exp)
    (expression ("%nameless-var" number) nameless-var-exp)
    (expression ("%let" expression "in" expression) nameless-let-exp)
    (expression ("%lexproc" expression) nameless-proc-exp)))

(sllgen:make-define-datatypes scanner-spec grammar)


; Debugging helpers for scanner and parser

(define list-the-datatypes
  (lambda()
    (sllgen:list-define-datatypes scanner-spec grammar)))

(define just-scan
  (sllgen:make-string-scanner scanner-spec grammar))

(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))

; ------------------------------------------------------------------------------
;; apply-procedure : Proc × ExpVal → ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (body saved-nameless-env)
                 (value-of body
                           (extend-nameless-env val saved-nameless-env))))))

; ------------------------------------------------------------------------------
; Expressed values

; We directly embed the two list constructors here, as an alternative we could
; have a single list-val wrapping a pair or the empty list. The version below
; is closer to a real implementation and avoids some ping-pong between types.
;; procedure : Nameless-exp × Nameless-env → Proc
(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))

(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (emptylist-val)
  (cons-val (first expval?) (rest expval?))
  (proc-val (proc proc?))
  (vector-val (vec vector?)))

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

(define expval->vector
  (lambda (val)
    (cases expval val
      (vector-val (vec) vec)
      (else (report-expval-extractor-error 'vector val)))))
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
                "<<procedure*>>")
      (vector-val (vector1)
                  vector1))))
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

;; value-of : Nameless-exp × Nameless-env → ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (nullary-exp (op)
                   ((nullary-table op)))
      (unary-exp (op exp1)
                 (let ((val1 (value-of exp1 nameless-env)))
                   ((unary-table op) val1)))
      (binary-exp (op exp1 exp2)
                  (let ((val1 (value-of exp1 nameless-env))
                        (val2 (value-of exp2 nameless-env)))
                    ((binary-table op) val1 val2)))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 nameless-env)))
                (if (expval->bool val1)
                    (value-of exp2 nameless-env)
                    (value-of exp3 nameless-env))))
      (cond-exp (lhss rhss)
                (value-of-cond lhss rhss nameless-env))
      (call-exp (rator rand)
                (let ((proc (expval->proc (value-of rator nameless-env)))
                      (args (value-of rand nameless-env)))
                  (apply-procedure proc args)))
      (nameless-var-exp (n) (apply-nameless-env nameless-env n))
      (nameless-let-exp (exp1 body)
                        (let ((val (value-of exp1 nameless-env)))
                          (value-of body
                                    (extend-nameless-env val nameless-env))))
      (nameless-proc-exp (body)
                         (proc-val
                          (procedure body nameless-env)))
      (else
       (report-invalid-translated-expression exp)))))

(define value-of-cond
  (lambda (lhss rhss env)
    (cond ((null? lhss)
           (eopl:error 'value-of-cond "No LHS evaluated to true"))
          ((expval->bool (value-of (car lhss) env))
           (value-of (car rhss) env))
          (else
           (value-of-cond (cdr lhss) (cdr rhss) env)))))

(define report-invalid-translated-expression
  (lambda (exp)
    (eopl:error 'value-of "Invalid translated expression: ~s" exp)))

;; translation-of : Exp × Senv → Nameless-exp
(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num) (const-exp num))
      (nullary-exp (op) (nullary-exp op))
      (unary-exp (op exp1)
                 (unary-exp
                  op
                  (translation-of exp1 senv)))
      (binary-exp (op exp1 exp2)
                  (binary-exp
                   op
                   (translation-of exp1 senv)
                   (translation-of exp2 senv)))
      (if-exp (exp1 exp2 exp3)
              (translation-of exp1 senv)
              (translation-of exp2 senv)
              (translation-of exp3 senv))
      (var-exp (var)
               (nameless-var-exp
                (apply-senv senv var)))
      (let-exp (var exp1 body)
               (nameless-let-exp
                (translation-of exp1 senv)
                (translation-of body
                                (extend-senv var senv))))
      (proc-exp (var body)
                (nameless-proc-exp
                 (translation-of body
                                 (extend-senv var senv))))
      (call-exp (rator rand)
                (call-exp
                 (translation-of rator senv)
                 (translation-of rand senv)))
      (else
       (report-invalid-source-expression exp)))))

(define report-invalid-source-expression
  (lambda (exp)
    (eopl:error 'value-of "Invalid source expression: ~s" exp)))

;; translation-of-program : Program → Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program
                  (translation-of exp1 (init-senv)))))))

;; value-of-program : Nameless-program → ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-nameless-env))))))

;; run : String → ExpVal
(define run
  (lambda (string)
    (value-of-program
     (translation-of-program
      (scan&parse string)))))

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

;(run "letrec
;          even(x) = if zero?(x) then 1 else (odd -(x,1))
;          odd(x) = if zero?(x) then 0 else (even -(x,1))
;      in (odd 13)")
;
;(run "letrec
;      one(x, y) = if zero?(x) then 1 else (two -(x, 1) y)
;      two(x, y) = if zero?(y) then 0 else (one x -(y, 1))
;       in (two 5 4)")
;
;(run "letrec
;      func(x) =
;       if zero?(x) then
;          1
;      else
;         -((func -(x, 1)), -(0, x))
;      in (func 10)")
;
;(run "let fact = proc (n) add1(n)
;        in let fact = proc (n)
;                          if zero?(n)
;                          then 1
;                          else *(n,(fact -(n,1)))
;          in (fact 5)")