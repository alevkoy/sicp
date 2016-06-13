; Exercise 1.17
(define (halve x) (/ x 2))
(define (double x) (+ x x))
(define (fast-mult a b)
 ; Rearrange to make sure b is nonnegative
 (cond ((< b 0) (fast-mult (- a) (- b)))
       ((= b 1) a)
       ((= b 0) 0)
       ((even? b) (fast-mult (double a) (halve b)))
       (else (+ a (fast-mult a (-1+ b))))))
