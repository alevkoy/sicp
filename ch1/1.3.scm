; Exercise 1.3
(define (f1.3 x y z)
        (define (square x) (* x x))
        (define (sum-of-squares x y) (+ (square x) (square y)))
        (define (<= x y) (or (< x y) (= x y)))
        (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
              ((and (<= y x) (<= y z)) (sum-of-squares x z))
              (else (sum-of-squares x y))))
