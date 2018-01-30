#lang eopl

;;number->sequence
;(define number->sequence
;  (lambda (n) (list n '() '())))
;
;;current-element
;(define current-element
;  (lambda (l)
;    (if (null? l)
;        (report-null-list)
;        (car l))))
;
;;move-to-left
;(define move-to-left
;  (lambda (seq)
;    (list
;     (car (left-sequence seq))
;     (cdr (left-sequence seq))
;     (cons (current-element seq) (right-sequence)))))
;
;;move-to-right
;(define move-to-right
;  (lambda (seq)
;    (list
;     (car (right-sequence seq))
;     (cons (current-element seq) (left-sequence seq))
;     (cdr (right-sequence seq)))))
;
;;insert-to-left
;(define insert-to-left
;  (lambda (val lst)
;    (list (current-element lst)
;          (cons val (left-sequence lst))
;          (right-sequence lst))))
;
;;insert-to-right
;(define insert-to-right
;  (lambda (val lst)
;    (list (current-element lst)
;          (left-sequence lst)
;          (cons val (right-sequence lst)))))
;;at-left-end?
;;at-right-end?
;
;(define right-sequence
;  (lambda (seq) (caddr seq)))
;
;(define left-sequence
;  (lambda (seq) (cadr seq)))
;
;(define report-null-list
;  (lambda ()
;    (eopl:error 'current-element "null list")))


; ------------------------------------------------------------------------------
; Exercise 2.18

(define number->sequence (lambda (n) (make-sequence n '() '())))

(define make-sequence
  (lambda (current-element left-part right-part)
    (list current-element left-part right-part)))

(define current-element car)
(define left-part cadr)
(define right-part caddr)

(define move-to-left
  (lambda (seq)
    (if (at-left-end? seq)
        (report-moving-too-far 'move-to-left)
        (make-sequence (car (left-part seq))
                       (cdr (left-part seq))
                       (cons (current-element seq) (right-part seq))))))

(define move-to-right
  (lambda (seq)
    (if (at-right-end? seq)
        (report-moving-too-far 'move-to-right)
        (make-sequence (car (right-part seq))
                       (cons (current-element seq) (left-part seq))
                       (cdr (right-part seq))))))

(define insert-to-left
  (lambda (n seq)
    (make-sequence (current-element seq)
                   (cons n (left-part seq))
                   (right-part seq))))

(define insert-to-right
  (lambda (n seq)
    (make-sequence (current-element seq)
                   (left-part seq)
                   (cons n (right-part seq)))))

(define at-left-end? (lambda (seq) (null? (left-part seq))))
(define at-right-end? (lambda (seq) (null? (right-part seq))))

(define report-moving-too-far
  (lambda (func)
    (eopl:error func "Moved too far")))

;; test
(define s1 (number->sequence 3))
(define s2 (insert-to-left 1 s1))
(define s3 (insert-to-right 5 s2))
