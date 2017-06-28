#lang eopl

;number->bintree
(define number->bintree
  (lambda (int) (make-bintree int '() '())))

(define make-bintree
  (lambda (node left-bintree right-bintree)
    (list node left-bintree right-bintree)))

;current-element
(define current-element
  (lambda (bintree) (car bintree)))

;move-to-left-son
(define move-to-left-son
  (lambda (bintree)
    (left-bintree bintree)))

;move-to-left
(define move-to-left
  (lambda (bintree)
    (if (at-leaf? (move-to-left-son bintree))
        (report-moving-too-far 'move-to-left)
        (make-bintree (car (left-bintree bintree))
                      (cdr (left-bintree bintree))
                      (make-bintree (current-element bintree) '() (right-bintree bintree))))))

;move-to-right
(define move-to-right
  (lambda (bintree)
    (if (at-leaf? (move-to-right-son bintree))
        (report-moving-too-far 'move-to-right)
        (make-bintree (car (right-bintree bintree))
                      (make-bintree (current-element bintree) (left-bintree bintree) '())
                      (cdr (left-bintree bintree))))))

;move-to-right-son
(define move-to-right-son
  (lambda (bintree)
    (right-bintree bintree)))

;at-leaf?
(define at-leaf?
  (lambda (bintree) (null? bintree)))
         
;insert-to-left
(define insert-to-left
  (lambda (node bintree)
    (list (current-element bintree)
          (make-bintree node (left-bintree bintree) '())
          (right-bintree bintree))))
     
;insert-to-right
(define insert-to-right
  (lambda (node bintree)
    (list (current-element bintree)
          (left-bintree bintree)
          (make-bintree node '() (right-bintree bintree)))))

(define left-bintree
  (lambda (bintree) (cadr bintree)))

(define right-bintree
  (lambda (bintree) (caddr bintree)))

(define report-moving-too-far
  (lambda (func)
    (eopl:error func "Moved too far")))
