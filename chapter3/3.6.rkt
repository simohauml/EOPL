#lang eopl
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
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("minus" "(" expression ")") minus-exp)))

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


;(define-datatype program program?
;  (a-program
;   (exp1 expression?)))
;
;(define-datatype expression expression?
;  (const-exp
;   (num number?))
;  (diff-exp
;   (exp1 expression?)
;   (exp2 expression?))
;  (zero?-exp
;   (exp1 expression?))
;  (if-exp
;   (exp1 expression?)
;   (exp2 expression?)
;   (exp3 expression?))
;  (var-exp
;   (var identifier?))
;  (let-exp
;   (var identifier?)
;   (exp1 expression?)
;   (body expression?)))
;
;(define identifier? symbol?)
;
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))


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

(define report-expval-extractor-error
  (lambda (expected val)
    (eopl:error 'expval-extractor "Expected a ~s, got ~s" expected val)))

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
                     (num-val (- num))))))))