#lang eopl

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
; Exercise 2.8

(define empty-env?
  (lambda (env)
    (null? env)))

; ------------------------------------------------------------------------------                       
; Exercise 2.9

(define has-binding?
  (lambda (initial-env search-var)
    (letrec ((loop (lambda (env)
                     (cond ((null? env) #f)
                           ((and (pair? env) (pair? (car env)))
                            (let ((saved-var (caar env))
                                  (saved-val (cdar env))
                                  (saved-env (cdr env)))
                              (if (eqv? search-var saved-var)
                                  #t
                                  (loop saved-env))))
                           (else
                            (report-invalid-env initial-env))))))
      (loop initial-env))))

; ------------------------------------------------------------------------------                       
; Exercise 2.10

(define extend-env*
  (lambda (vars vals env)
    (cond ((and (null? vars) (null? vals))
           env)
          ((not (eq? (length vars) (length vals)))
          (length-not-eqv vars vals))
          ((and (pair? vars) (pair? vals))
           (extend-env* (cdr vars)
                               (cdr vals)
                               (extend-env (car vars) (car vals) env)))
          ((null? vars)
           (report-too-few-variables))
          (else
           (report-too-few-values)))))

(define report-too-few-variables
  (lambda ()
    (eopl:error 'extend-env* "Too few variables")))

(define report-too-few-values
  (lambda ()
    (eopl:error 'extend-env* "Too few values")))

(define length-not-eqv
  (lambda (vars vals)
    (eopl:error 'extend-env* "Length of ~s and ~s are not equal" vars vals)))

