#lang eopl

; ------------------------------------------------------------------------------
; Exercise 2.4
;
; (empty-stack)            = |[]|
; (push v |[v1 v2 v3...]|) = |[v v1 v2 v3...]|
; (pop |[v1 v2 v3...]|)    = |[v2 v3...]|
; (top |[v1 v2 v3...]|)    = v1
; (empty-stack? s)         = #t if s = |[]|, else #f
;
; constructors: empty-stack, push
; observers: pop, top, empty-stack?
(define scheme-value? (lambda (s) #t))

(define-datatype stack stack?
  (empty-stack)
  (push
   (new scheme-value?)
   (saved-stack stack?)))

(define pop
  (lambda (s)
    (cases stack s
      (empty-stack () (report-empty-stack pop))
      (push (val stk) stk))))

(define top
  (lambda (s)
    (cases stack s
      (empty-stack () (report-empty-stack top))
      (push (val stk) val))))

(define empty-stack?
  (lambda (s)
    (cases stack s
      (empty-stack () #t)
      (else #f))))

(define report-empty-stack
  (lambda (func)
    (eopl:error func "The stack is empty")))

;; test
(define s1 (push 'a
                 (push 'b
                       (push 'c
                             (empty-stack)))))

(equal? (top s1) 'a)