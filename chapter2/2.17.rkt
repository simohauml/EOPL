#lang racket

;; var-exp : Var -> Lc-exp
(define var-exp
  (lambda (var)
    (cons 'var-exp var)))

;; lambda-exp : Var * Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (var lc-exp)
    (list 'lambda-exp (list var) lc-exp)))

;; app-exp : Lc-exp * Lc-exp -> Lc-exp
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list 'app-exp lc-exp1 lc-exp2)))

;; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'var-exp))))

;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (x)
    (and (pair? x) (eq? (car x) 'lambda-exp))))

;; app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (x)
    (and (list? x) (eq? (car x) 'app-exp))))

;; var-exp->var : Lc-exp -> Var
(define var-exp->var
  (lambda (x)
    (cdr x)))

;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var
  (lambda (x)
    (caadr x)))

;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (x)
    (caddr x)))

;; app-exp->rator : Lc-exp -> Lc-exp
(define app-exp->rator
  (lambda (x)
    (cadr x)))

;; app-exp->rand : Lc-exp -> Lc-exp
(define app-exp->rand
  (lambda (x)
    (caddr x)))

;; occurs-free? : Sym * Lcexp -> Bool
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

(equal??
 (occurs-free? 'a (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
 #f)

(equal??
 (occurs-free? 'b (lambda-exp 'a (app-exp (var-exp 'b) (var-exp 'a))))
 #t)






; ------------------------------------------------------------------------------
; Exercise 2.17
; a) As a list with a tag as the first element:
(define var-exp-a (lambda (var) (list 'var var)))
(define lambda-exp-a (lambda (bound-var body) (list 'lambda bound-var body)))
(define app-exp-a (lambda (exp1 exp2) (list 'app exp1 exp2)))

(define var-exp?-a (lambda (exp) (and (pair? exp) (eqv? 'var (car exp)))))
(define lambda-exp?-a (lambda (exp) (and (pair? exp) (eqv? 'lambda (car exp)))))
(define app-exp?-a (lambda (exp) (and (pair? exp) (eqv? 'app (car exp)))))

(define var-exp->var-a (lambda (exp) (cadr exp)))
(define lambda-exp->bound-var-a (lambda (exp) (cadr exp)))
(define lambda-exp->body-a (lambda (exp) (caddr exp)))
(define app-exp->rator-a (lambda (exp) (cadr exp)))
(define app-exp->rand-a (lambda (exp) (caddr exp)))

; b) As symbol/vector/pair:
(define var-exp-b (lambda (var) var))
(define lambda-exp-b (lambda (bound-var body) (vector bound-var body)))
(define app-exp-b (lambda (exp1 exp2) (cons exp1 exp2)))

(define var-exp?-b (lambda (exp) (symbol? exp)))
(define lambda-exp?-b (lambda (exp) (vector? exp)))
(define app-exp?-b (lambda (exp) (pair? exp)))

(define var-exp->var-b (lambda (exp) exp))
(define lambda-exp->bound-var-b (lambda (exp) (vector-ref exp 0)))
(define lambda-exp->body-b (lambda (exp) (vector-ref exp 1)))
(define app-exp->rator-b (lambda (exp) (car exp)))
(define app-exp->rand-b (lambda (exp) (cdr exp)))



