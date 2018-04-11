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
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)))

(define grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    ;(expression ("+" "(" expression "," expression ")") addi-exp)
    ;(expression ("*" "(" expression "," expression ")") mult-exp)
    ;(expression ("/" "(" expression "," expression ")") quot-exp)
    ;(expression ("equal?" "(" expression "," expression ")") equal-exp)
    ;(expression ("greater?" "(" expression "," expression ")") greater-exp)
    ;(expression ("less?" "(" expression "," expression ")") less-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("null?" expression) nulllist?-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)
    ;(expression ("(" expression (arbno expression) ")") call-exp)
    (expression ("(" expression expression")") call-exp)
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

;; value-of : Nameless-exp × Nameless-env → ExpVal
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (diff-exp (exp1 exp2)
                (num-val (- (expval->num (value-of exp1 nameless-env))
                            (expval->num (value-of exp2 nameless-env)))))
      (zero?-exp (exp1) (bool-val (value-of exp1 nameless-env)))
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
      (diff-exp (exp1 exp2)
                (diff-exp
                 (translation-of exp1 senv)
                 (translation-of exp2 senv)))
      (zero?-exp (exp1)
                 (zero?-exp
                  (translation-of exp1 senv)))
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