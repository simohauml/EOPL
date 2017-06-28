#lang eopl
; ------------------------------------------------------------------------------                       
; Exercise 2.3

(define zero (lambda () '(diff (one) (one))))
;(define is-zero?
;  (lambda (n)
;    (letrec ((loop
;              (lambda (n)
;                (if (eqv? (car n) 'one)
;                    1
;                    (- (loop (cadr n)) (loop (caddr n)))))))
;      (eqv? 0 (loop n)))))
(define is-zero?
  (lambda (n)
    (= 0 (calc n))))
(define calc
  (lambda (n)
    (if (eqv? (car n) 'one)
        1
        (- (calc (cadr n)) (calc (caddr n))))))
(define successor
  (lambda (n)
    (list 'diff n '(diff (diff (one) (one)) (one)))))
(define predecessor
  (lambda (n)
    (list 'diff n '(one))))

(define diff-tree-plus
  (lambda (sum add)
    (list 'diff sum (list 'diff (zero) add))))

(define diff-tree-minus
  (lambda (minu sub)
    (list 'diff minu sub)))