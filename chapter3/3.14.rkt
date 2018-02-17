#lang eopl

; ------------------------------------------------------------------------------
; Environments

; See: exercise 2.5

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

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
; Scanner and parser specification

(define scanner-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (nullary-op ("emptylist") string)
    (unary-op ((or "minus" "car" "cdr")) string)
    (binary-op ((or "+" "-" "*" "/" "cons")) string)
    (n-ary-op ("list") string)
    (unary-bool-op ((or "zero?" "null?")) string)
    (binary-bool-op ((or "equal?" "greater?" "less?")) string)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)))

(define grammar
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (nullary-op) nullary-exp)
    (expression (unary-op "(" expression ")") unary-exp)
    (expression (binary-op "(" expression "," expression ")") binary-exp)
    (expression (n-ary-op "(" (separated-list expression ",") ")") n-ary-exp)
    (expression ("if" bool-exp "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno bool-exp "==>" expression) "end") cond-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (bool-exp (unary-bool-op "(" expression ")") unary-bool-exp)
    (bool-exp (binary-bool-op "(" expression "," expression ")") binary-bool-exp)))

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
; Expressed values

; We directly embed the two list constructors here, as an alternative we could
; have a single list-val wrapping a pair or the empty list. The version below
; is closer to a real implementation and avoids some ping-pong between types.
(define-datatype expval expval?
  (num-val (num number?))
  ;;(bool-val (bool boolean?))
  ;;(emptylist-val)
  ;;(cons-val (first expval?) (rest expval?))
  )

(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

(define expval->bool
  (lambda (val)
    (cond
      ((boolean? val) val)
      ((zero? (expval->num val)) #f)
      (else #t))))
;
;(define expval->car
;  (lambda (val)
;    (cases expval val
;      (cons-val (first rest) first)
;      (else (report-expval-extractor-error 'cons val)))))
;
;(define expval->cdr
;  (lambda (val)
;    (cases expval val
;      (cons-val (first rest) rest)
;      (else (report-expval-extractor-error 'cons val)))))
;
;(define expval->emptylist?
;  (lambda (val)
;    (cases expval val
;      (emptylist-val () #t)
;      (cons-val (first rest) #f)
;      (else (report-expval-extractor-error 'cons-or-emptylist val)))))

; No 'foldr' in #lang eopl, use a specialized version for 1 list.
(define foldr-1
  (lambda (func init lst)
    (if (null? lst)
        init
        (func (car lst) (foldr-1 func init (cdr lst))))))

(define list-val
  (lambda (elements)
    (foldr-1 cons empty elements)))

(define report-expval-extractor-error
  (lambda (expected val)
    (eopl:error 'expval-extractor "Expected a ~s, got ~s" expected val)))

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
    (cond
      ((bool-exp? exp) (value-of-bool-exp exp env))
      ((expression? exp) (value-of-exp exp env))
      (else (report-nosupport-exp-error exp)))))

(define value-of-exp
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
                 (value-of body (extend-env var val1 env)))))))

(define value-of-cond
  (lambda (lhss rhss env)
    (cond ((null? lhss)
           (eopl:error 'value-of-cond "No LHS evaluated to true"))
          ((value-of (car lhss) env)
           (value-of (car rhss) env))
          (else
           (value-of-cond (cdr lhss) (cdr rhss) env)))))

(define value-of-bool-exp
  (lambda (exp env)
    (cases bool-exp exp
      (unary-bool-exp (op exp1)
                      (let ((val1 (value-of exp1 env)))
                        (expval->bool val1)))
      (binary-bool-exp (op exp1 exp2)
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
                       ((binary-table op) val1 val2))))))
                        
(define report-nosupport-exp-error
  (lambda (exp)
    (eopl:error 'value-of "Expression: ~s is not supported" exp)))
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
      (func (expval->num val)))))

(define unary-arithmetic-func
  (lambda (func)
    (lambda (val)
      (num-val (func (expval->num val))))))

(define unary-list-predicate
  (lambda (func)
    (lambda (val)
      (func val))))

(define binary-relation
  (lambda (func)
    (lambda (val1 val2)
      (func (expval->num val1) (expval->num val2)))))

(define binary-arithmetic-func
  (lambda (func)
    (lambda (val1 val2)
      (num-val (func (expval->num val1) (expval->num val2))))))

; ------------------------------------------------------------------------------
; Tables of n-ary operations.

(define nullary-table
  (make-table (entry "emptylist" (lambda () empty))))
  
(define unary-table
  (make-table (entry "zero?" (unary-arithmetic-predicate zero?))
              (entry "minus" (unary-arithmetic-func -))
              (entry "car" car)
              (entry "cdr" cdr)
              (entry "null?" null?)))

(define binary-table
  (make-table (entry "+" (binary-arithmetic-func +))
              (entry "-" (binary-arithmetic-func -))
              (entry "*" (binary-arithmetic-func *))
              (entry "/" (binary-arithmetic-func quotient))
              (entry "equal?" (binary-relation =))
              (entry "greater?" (binary-relation >))
              (entry "less?" (binary-relation <))
              (entry "cons" cons)))

(define n-ary-table
  (make-table (entry "list" list-val)))

; ------------------------------------------------------------------------------
;; initial environment
(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
                (extend-env 'v (num-val 5)
                            (extend-env 'x (num-val 10)
                                        (empty-env))))))
; ------------------------------------------------------------------------------
; A nice REPL for interactive use

(define read-eval-print
  (sllgen:make-rep-loop "-->" value-of-program
                        (sllgen:make-stream-parser scanner-spec grammar)))

;; test
(run "x")
(run "v")
(run "i")
(run "10")
(run "-(1, 2)")
(run "-(1, x)")

; (run "foo") -> error

(run  "if zero?(-(11, 11)) then 3 else 4")
(run "minus(4)")
(run  "if zero?(-(11, 11)) then minus(3) else minus(4)")

(run "+(1, 2)")
(run "+(+(1,2), 3)")
(run "/(1, 2)")
(run "*(*(1,2), *(10, 2))")

(run "if less?(1, 2) then 1 else 2")
(run "if greater?(2, 1) then minus(1) else minus(2)")

(run "1")

;(run "less?(1, 2)")
(run "cond less?(1, 2) ==> 2 end")
(run "cond less?(2, 1) ==> 1 greater?(2, 2) ==> 2  greater?(3, 2) ==> 3 end")
;; (run "cond less?(2, 1) ==> 1 end")  ==> error