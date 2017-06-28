#lang eopl

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define parse-prefix
  (lambda (datum)
    (if (prefix-exp? (car datum)) (car datum)
        (parse-prefix (rematch-prefix (match-prefix datum))))))

(define match-prefix
  (lambda (datum)
    (if (null? datum) '()
        (if (and (eqv? '- (car datum))
                (integer? (cadr datum))
                (integer? (caddr datum)))
            (cons (diff-exp (const-exp (cadr datum)) (const-exp (caddr datum)))
                 (match-prefix (cdddr datum)))
            (cons (car datum)
                  (match-prefix (cdr datum)))))))

(define rematch-prefix
  (lambda (datum)
    (if (null? datum) '()
        (cond
          ((and (eqv? '- (car datum))
                (integer? (cadr datum))
                (prefix-exp? (caddr datum)))
           (cons (diff-exp (const-exp (cadr datum)) (caddr datum))
                 (rematch-prefix (cdddr datum))))
          ((and (eqv? '- (car datum))
                (prefix-exp? (cadr datum))
                (integer? (caddr datum)))
           (cons (diff-exp (cadr datum) (const-exp (caddr datum)))
                 (rematch-prefix (cdddr datum))))
          ((and (eqv? '- (car datum))
                (prefix-exp? (cadr datum))
                (prefix-exp? (caddr datum)))
           (cons (diff-exp (cadr datum) (caddr datum))
                 (rematch-prefix (cdddr datum))))
          (else
           (cons (car datum)
                 (rematch-prefix (cdr datum))))))))


(define syntax-error
  (lambda (datum message-suffix)
      (eopl:error parse-prefix
                  (string-append "Syntax error: ~s is " message-suffix) datum)))


(define cal '(- - 3 2 - 4 - 12 7))
