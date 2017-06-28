#lang eopl

;nth-element : List * Int ---> SchemeVal
;usage: (nth-element lst n) = the n-th element of lst
(define nth-element
  (lambda (lst n)
    (nth-element-helper lst n lst n)))

(define nth-element-helper
  (lambda (orig-lst orig-n lst n)
    (if (null? lst)
        (report-list-does-not-have orig-lst orig-n)
        (if (zero? n)
            (car lst)
            (nth-element-helper orig-lst orig-n (cdr lst) (- n 1))))))

(define report-list-does-not-have
  (lambda (lst n)
    (eopl:error 'nth-element "~s does not have ~s elements.~%" lst (+ n 1))))

