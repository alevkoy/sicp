; Exercise 1.42

(define (compose f g)
    (lambda (x) (f (g x))))

(newline)
(display ((compose square 1+) 6))
