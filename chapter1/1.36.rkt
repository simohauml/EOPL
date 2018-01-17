#lang eopl

(define number-elements
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst))
           (number-elements (cdr lst))))))

(define g
  (lambda (num-and-sexp lst)
    (if (null? lst)
        (list num-and-sexp)
        (letrec ((loop (lambda (lst)
                         (cond
                           ((null? lst) '())
                           (else
                            (cons (cons (+ 1 (caar lst))
                                        (cdar lst))
                                  (loop (cdr lst))))))))
          (cons num-and-sexp
                (loop lst))))))

;; test
(define l1 '(a b c d e f g))
(define l2 '((0 a) (1 b) (2 c) (3 d)))
(number-elements l1)