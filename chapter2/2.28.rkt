#lang eopl

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

(define identifier?
  (lambda (x)
    (and (symbol? x) (not (eqv? 'lambda x)))))

(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp (bound-var body)
                  (list 'lambda (list bound-var)
                        (unparse-lc-exp body)))
      (app-exp (rator rand)
               (list
                (unparse-lc-exp rator) (unparse-lc-exp rand))))))

;; test
(define exp1 (lambda-exp 'a (app-exp (var-exp 'a) (var-exp 'b))))
(equal? (unparse-lc-exp exp1) '(lambda (a) (a b)))