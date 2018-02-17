#lang eopl

; ------------------------------------------------------------------------------
; Environments
; Exercise 2.21
(define-datatype env env?
  (empty-env)
  (extend-env
   (saved-var symbol?)
   (saved-val scheme-value?)
   (saved-env env?)))

(define apply-env
  (lambda (e search-var)
    (cases env e
      (empty-env () (report-no-binding-found search-var))
      (extend-env (s-var s-val s-env)
                  (if (eqv? search-var s-var)
                      s-val
                      (apply-env s-env search-var))))))

(define has-binding?
  (lambda (initial-env search-var)
    (cases env initial-env
      (empty-env () #f)
      (extend-env (s-var s-val s-env)
                  (if (eqv? search-var s-var)
                      #t
                      (has-binding? s-env search-var))))))

(define scheme-value? (lambda (s) #t))


(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))


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
    (expression ("+" "(" expression "," expression ")") addi-exp)
    (expression ("*" "(" expression "," expression ")") mult-exp)
    (expression ("/" "(" expression "," expression ")") quot-exp)
    (expression ("equal?" "(" expression "," expression ")") equal-exp)
    (expression ("greater?" "(" expression "," expression ")") greater-exp)
    (expression ("less?" "(" expression "," expression ")") less-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("null?" expression) nulllist?-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)))

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
; Expression datatype
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (emptylist-val)
  (cons-val
   (first expval?)
   (rest expval?)))

;expval->num : ExpVal → Int
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

;expval->bool : ExpVal → Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else
       (report-expval-extractor-error 'bool val)))))

;expval->bool : ExpVal → Bool
(define expval->list
  (lambda (val)
    (cases expval val
      (emptylist-val () '())
      (cons-val (first rest)
                (list first (expval->list rest)))
      (else
       (report-expval-extractor-error 'list val)))))

(define expval->emptylist?
  (lambda (val)
    (cases expval val
      (emptylist-val () #t)
      (cons-val (first rest) #f)
      (else (report-expval-extractor-error 'cons-or-emptylist val)))))

(define expval->car
  (lambda (val)
    (cases expval val
      (emptylist-val () (report-expval-EmptyList-error val))
      (cons-val (first rest) first)
      (else (report-expval-extractor-error 'cons-or-emptylist val)))))

(define expval->cdr
  (lambda (val)
    (cases expval val
      (emptylist-val () (report-expval-EmptyList-error val))
      (cons-val (first rest) rest)
      (else (report-expval-extractor-error 'cons-or-emptylist val)))))

(define foldr-1
  (lambda (func init lst)
    (if (null? lst)
        init
        (func (car lst) (foldr-1 func init (cdr lst))))))

(define list-val
  (lambda (elements)
    (foldr-1 cons-val (emptylist-val) elements)))

(define report-expval-extractor-error
  (lambda (expected val)
    (eopl:error 'expval-extractor "Expected a ~s, got ~s" expected val)))

(define report-expval-EmptyList-error
  (lambda (val)
    (eopl:error 'expval->car "List ~s is empty" val)))

;================================================================
;run : String → ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;value-of-program : Program → ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (addi-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (+ num1 num2)))))
      (mult-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (* num1 num2)))))
      (quot-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (/ num1 num2)))))
      (equal-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (bool-val
                     (= num1 num2)))))
      (greater-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (bool-val
                     (> num1 num2)))))
      (less-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (bool-val
                     (< num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))
      (minus-exp (exp)
                 (let ((val (value-of exp env)))
                   (let ((num (expval->num val)))
                     (num-val (- num)))))
      (emptylist-exp () (emptylist-val))
      (nulllist?-exp (exp)
                 (let ((val1 (value-of exp env)))
                         (let ((bool1 (expval->emptylist? val1)))
                           (bool-val bool1))))
      (cons-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (cons-val val1 val2)))
      (car-exp (exp)
               (let ((val1 (value-of exp env)))
                 (expval->car val1)))
      (cdr-exp (exp)
               (let ((val1 (value-of exp env)))
                 (expval->cdr val1)))
      (list-exp (exps)
                (list-val (map (lambda (expr) (value-of expr env)) exps))))))

; ------------------------------------------------------------------------------
;init-env : () → Env
;usage: (init-env) = [i=1,v=5,x=10]
(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;; test
(run "let x = 4
      in list(x, -(x,1), -(x,3))")