#lang eopl

;var-exp
(define var-exp (lambda (var) var))

;lambda-exp
(define lambda-exp
  (lambda (bound-var body) (list 'lambda bound-var body)))

(define app-exp (lambda (exp1 exp2) (list exp1 exp2)))

(define var-exp? (lambda (exp) (symbol? exp)))

(define lambda-exp?
  (lambda (exp) (and (pair? exp) (eqv? 'lambda (car exp)))))

(define app-exp?
  (lambda (exp) (and (pair? exp) (pair? (cdr exp) (null? (cddr exp))))))

(define var-exp->var (lambda (exp) exp))
(define lambda-exp->bound-var (lambda (exp) (cadr exp)))
(define lambda-exp->body (lambda (exp) (caddr exp)))
(define app-exp->rator (lambda (exp) (car exp)))
(define app-exp->rand (lambda (exp) (cadr exp)))

;occurs-free? : Sym × LcExp → Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))