#lang eopl

;Env = (empty-env) | (extend-env Var SchemeVal Env)
;Var = Sym

;;;empty-env : () → Env
;(define empty-env
;  (lambda () (list 'empty-env)))
;
;;extend-env : Var × SchemeVal × Env → Env
;(define extend-env
;  (lambda (var val env)
;    (list 'extend-env var val env)))
;
;;apply-env : Env × Var → SchemeVal
;(define apply-env
;  (lambda (env search-var)
;    (cond
;      ((eqv? (car env) 'empty-env)
;       (report-no-binding-found search-var))
;      ((eqv? (car env) 'extend-env)
;       (let ((saved-var (cadr env))
;             (saved-val (caddr env))
;             (saved-env (cadddr env)))
;         (if (eqv? search-var saved-var)
;             saved-val
;             (apply-env saved-env search-var))))
;      (else
;       (report-invalid-env env)))))
;
;(define report-no-binding-found
;  (lambda (search-var)
;    (eopl:error 'apply-env "No binding for ~s" search-var)))
;
;(define report-invalid-env
;  (lambda (env)
;    (eopl:error 'apply-env "Bad environment: ~s" env)))

(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

(define apply-env
  (lambda (initial-env search-var)
    (letrec ((loop (lambda (env)
                     (cond ((eqv? (car env) 'empty-env)
                            (report-no-binding-found search-var initial-env))
                           ((eqv? (car env) 'extend-env)
                            (let ((saved-var (cadr env))
                                  (saved-val (caddr env))
                                  (saved-env (cadddr env)))
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


(define e
  (extend-env 'd 6
      (extend-env 'y 8
         (extend-env 'x 7
            (extend-env 'y 14
                (empty-env))))))

(equal? (apply-env e 'd) 6)
(equal? (apply-env e 'y) 8)
(equal? (apply-env e 'x) 7)
