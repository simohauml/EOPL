#lang eopl

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "No binding for ~s" search-var)))

;(define empty-stack
;  (lambda () '()))
;
;(define push
;  (lambda (val stack)
;    (cons val stack)))
;
;(define pop
;  (lambda (stack)
;    (if (empty-stack? stack)
;        empty-stack
;        (cdr stack))))
;
;(define top
;  (lambda (stack)
;    (if (empty-stack? stack)
;        empty-stack
;        (car stack))))
;
;(define empty-stack?
;  (lambda (stack)
;    (null? stack)))

;(define empty-stack
;  (lambda ()
;    (lambda ()
;      '())))
;
;(define push
;  (lambda (val stack)
;    (lambda (s)
;      (

(define empty-stack
  (lambda()
    (lambda (cmd)
      (cond
       ((eqv? cmd 'top)
        (eopl:error 'top "try top on empty stack"))
       ((eqv? cmd 'pop)
        (eopl:error 'pop "try pop on empty stack"))
       (else
        (eopl:error "unknow cmd on stack"))))))

(define push
  (lambda (saved-stack var)
    (lambda (cmd)
      (cond
       ((eqv? cmd 'top) var)
       ((eqv? cmd 'pop) saved-stack)
       (else
	(eopl:error "error cmd"))))))

(define pop
  (lambda (stack)
    (stack 'pop)))

(define top
  (lambda (stack)
    (stack 'top)))


;----------------------------------------------------
; A variant using a triple of functions.
;(define make-stack
;  (lambda (pop-func top-func empty-stack?-func)
;    (list pop-func top-func empty-stack?-func)))
;
;(define pop-func car)
;(define top-func cadr)
;(define empty-stack?-func caddr)
;
;(define empty-stack
;  (lambda ()
;    (make-stack
;     (lambda () (report-empty-stack 'pop))
;     (lambda () (report-empty-stack 'top))
;     (lambda () #t))))
;
;(define push
;  (lambda (stack value)
;    (make-stack
;     (lambda () stack)
;     (lambda () value)
;     (lambda () #f))))
;
;(define pop
;  (lambda (stack)
;    ((pop-func stack))))
;
;(define top
;  (lambda (stack)
;    ((top-func stack))))
;
;(define empty-stack?
;  (lambda (stack)
;    ((empty-stack?-func stack))))
;
;(define report-empty-stack
;  (lambda (func)
;    (eopl:error func "Empty stack")))
;
;; A variant using a more "OO"-like interface.
;(define empty-stack-oo
;  (lambda ()
;    (letrec ((this (lambda (message . args)
;                     (case message
;                       ((push) (push-oo this (car args)))
;                       ((pop) (report-empty-stack 'pop))
;                       ((top) (report-empty-stack 'top))
;                       ((empty-stack?) #t)))))
;      this)))
;
;(define push-oo
;  (lambda (stack value)
;    (letrec ((this (lambda (message . args)
;                     (case message
;                       ((push) (push-oo this (car args)))
;                       ((pop) stack)
;                       ((top) value)
;                       ((empty-stack?) #f)))))
;      this)))
