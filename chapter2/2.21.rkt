#lang eopl

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

;; test
(define e1 (extend-env 'a 1
                       (extend-env 'b 2
                                   (extend-env 'c 3
                                               (empty-env)))))

(equal? (has-binding? e1 'a) #t)