; Exercise 1.7
(define (average x y) (/ (+ x y) 2))

(define (better-enough? guess prev)
 (< (/ (abs (- guess prev)) guess) 0.00001))

(define (sqrt x)
  (define (sqrt-iter guess prev)
    (if (better-enough? guess prev)
      guess
      (sqrt-iter (improve guess) guess)))

  (define (improve guess) (average guess (/ x guess)))

  (sqrt-iter 1.0 0.0))
