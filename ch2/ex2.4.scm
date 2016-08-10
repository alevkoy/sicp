; Exercise 2.4
; cons and car adapted from SICP.

(define (cons x y)
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

(define x (cons 5 3))
(newline)
(display (cdr x))
