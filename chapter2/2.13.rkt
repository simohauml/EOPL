#lang eopl

;Env = Var → SchemeVal
;empty-env : () → Env
;(define empty-env
;  (lambda ()
;    (lambda (search-var)
;      (report-no-binding-found search-var))))

;extend-env : Var × SchemeVal × Env → Env
;(define extend-env
;  (lambda (saved-var saved-val saved-env)
;    (lambda (search-var)
;      (if (eqv? search-var saved-var)
;          saved-val
;          (apply-env saved-env search-var)))))

;Env = Var → SchemeVal
;empty-env : () → Env
(define empty-env
  (lambda ()
    (cons
     (lambda (search-var)
       (report-no-binding-found search-var))
     (lambda () #t))))

;extend-env : Var × SchemeVal × Env → Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (cons
     (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))
     (lambda () #f))))

;empty-env? : Env → Bool
(define empty-env?
  (lambda (env) ((cdr env))))

;apply-env : Env × Var → SchemeVal
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

;; test
(define e (extend-env 'a 1
                      (extend-env 'b 2
                                  (extend-env 'c 3
                                              (empty-env)))))