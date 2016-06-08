; Exercise 1.8
(define (cuberoot x)
  (define (cuberoot-iter guess prev)
    (if (better-enough? guess prev)
         guess
         (cuberoot-iter (improve guess) guess)))

  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

  (cuberoot-iter 1.0 0.0))
