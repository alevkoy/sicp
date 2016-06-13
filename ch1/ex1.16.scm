; Exercise 1.16
(define (expt b n)
 ; Iteration invariant: a*b^n
 (define (expt-iter b n a)
  ; Algorithm doesn't work with negative n, because decrementing
  ; needs to take n closer to 0. Handle that by negating n and
  ; inverting b if n is negative. This won't change the result,
  ; but it will make n nonnegative.
  (cond ((< n 0) (expt-iter (/ 1 b) (- n) a))
        ((= n 0) a)
        ((even? n) (expt-iter (square b) (/ n 2) a))
        (else (expt-iter b (-1+ n) (* a b)))))
 (expt-iter b n 1))
