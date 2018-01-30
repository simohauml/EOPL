#lang eopl

; ------------------------------------------------------------------------------
; Exercise 2.1
(define N 2)
(define zero (lambda () '()))
(define is-zero? (lambda (n) (null? n)))
(define successor
  (lambda (n)
    (cond
      ((is-zero? n) '(1))
      ((< (+ 1 (car n)) N) (cons (+ 1 (car n)) (cdr n)))
      (else (cons 0 (successor (cdr n)))))))
(define predecessor
  (lambda (n)
    (cond
      ((is-zero? n) (zero))
      ((> (- (car n) 1) 0) (cons (- (car n) 1) (cdr n)))
      ((and (= (car n) 1) (not (is-zero? (cdr n)))) (cons (- (car n) 1) (cdr n)))
      ((and (= (car n) 1) (is-zero? (cdr n))) (zero))
      (else (cons (- N 1) (predecessor (cdr n)))))))

(define factorial
  (lambda (n)
    (if (= 0 n)
        0
        (if (= 1 n)
            1
            (* n (factorial (- n 1)))))))

(define plus-bigit
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (plus-bigit (predecessor x) y)))))

(define times-bigit
  (lambda (m n)
    (if (is-zero? m)
        (zero)
        (plus-bigit n (times-bigit (predecessor m) n)))))

(define fact-bigit
  (lambda (n)
    (if (is-zero? n)
        (zero)
        (if (is-zero? (predecessor n))
            n
            (times-bigit n (fact-bigit (predecessor n)))))))

(define make
  (lambda (n)
    (if (zero? n)
        '()
        (if (odd? n)
            (successor (make (- n 1)))
            (times-bigit (make (/ n 2)) '(2))))))

;; test
(factorial 10)
(fact-bigit '(10))

; ------------------------------------------------------------------------------
; Exercise 2.2

;
; The unary representation is quite efficient, but fulfills the specification.
; The representation using Scheme number is only correct iff the underlying
; Scheme numbers support arbitrary precision integers. The bignum representation
; is relatively efficient, especially for larger N, and fulfills the
; specification.

;We will analyze the implementations from a client's viewpoint, plus in this case:
;
;(define plus
;  (lambda (x y)
;    (if (iszero? x)
;        y
;        (succ (plus (pred x) y)))))
;
;;Unary reprentation
;(define zero '())
;(define iszero? null?)
;(define succ
;  (lambda (n) (cons #t n)))
;(define pred cdr)

;The unary representation is the simplest of our implementation since it just maintains a list of #t's
;representing the positive integers. All four operation implementations have constant access time.
;The downside is the space required, lineal in the number represented. This becomes unsustainable for
;large numbers.


;Scheme number representation
;(define zero 0)
;(define iszero? zero?)
;(define succ
;  (lambda (n) (+ n 1)))
;(define pref
;  (lambda (n) (- n 1)))

;Here we are just delegating the implementation to that of Scheme. We are actually introducing a new
;abstraction layer that may become helpful in the case we need to change the representation of the
;data specification without affecting the clients (e.g. for performance purposes).
;The space required to represent a number is likely optimized on the underlying Scheme internal
;representation, especially for big numbers.
;It seems that, since pred delegates to Scheme's internal representation, (pred zero) returns -1 and
;this would invalidate the implementation. But as stated in the text, the specifications says
;nothing about (pred zero) and any behavior is acceptable.

;Bignum representation
;The implementation of the operations can be found in exercise 2.1., along with related comments.
;As explained in the text: "The most efficient representation is often a lot more difficult to
;implement, so we may wish to develop a simple implementation first and only change to a more
;efficient representation if it proves critical to the overall performance of a system". We verified
;this when we implemented bigints, as it turned out that it took a lot more time to develop a
;working implementation, albeit more efficient.

;Church numerals
;In exercise 2.6 of SICP, we implement a representation of the nonnegative integers using only
;lambda expressions:

;(define zero (lambda (f) (lambda (x) x)))
;
;(define (add-1 n) (lambda (f) (lambda (x) (f ((n f) x)))))

;As we can see, we just need the concept of lambda expressions to represent the positives.

