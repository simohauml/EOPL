#lang eopl

;Env = Var → SchemeVal
;empty-env : () → Env
(define empty-env
  (lambda ()
    (list
      (lambda (search-var) (report-no-binding-found search-var))
      (lambda () #t)
      (lambda (search-var) #f))))

;extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))
     (lambda () #f)
     (lambda (search-var)
       (if (eqv? search-var saved-var)
           #t
           (has-binding? saved-env search-var))))))

;empty-env? : Env → Bool
(define empty-env?
  (lambda (env) ((cadr env))))

;has-binding? : Env × Var → Bool
(define has-binding?
  (lambda (env var) ((caddr env) var)))

;apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var) ((car env) search-var)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

;; test
(define e (extend-env 'a 1
                      (extend-env 'b 2
                                  (extend-env 'c 3
                                              (empty-env)))))

(equal? (has-binding? e 'b) #t)