; Exercise 1.41

(define (double proc)
    (lambda (x) (proc (proc x))))

(newline)
(display ((double 1+) 2))

(newline)
(display (((double (double double)) 1+) 5))
