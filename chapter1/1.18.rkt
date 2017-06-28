#lang eopl

;swapper : List ---> List
;usage : (swapper s1 s2 slist)

(define swapper
  (lambda (s1 s2 slist)
    (if (null? slist)
        '()
        (if (symbol? (car slist))
           (cond
             [(eqv? s1 (car slist)) (cons s2 (swapper s1 s2 (cdr slist)))]
             [(eqv? s2 (car slist)) (cons s1 (swapper s1 s2 (cdr slist)))]
             [else (cons (car slist) (swapper s1 s2 (cdr slist)))])
           (cons
            (swapper s1 s2 (car slist))
            (swapper s1 s2 (cdr slist)))))))

; swapper using map
(define swapper2
  (lambda (s1 s2 slist)
    (map (lambda (sexp) (swap-in-s-exp s1 s2 sexp)) slist)))

(define swap-in-s-exp
  (lambda (s1 s2 sexp)
    (cond ((not (symbol? sexp)) (swapper2 s1 s2 sexp))
          ((eqv? s1 sexp) s2)
          ((eqv? s2 sexp) s1)
          (else sexp))))