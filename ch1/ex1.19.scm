; Exercise 1.19
(define (fib n)
 (define (fib-iter a b p q count)
  (cond (( = count 0)
         b)
        (( < count 0) 0)
        ((even? count)
         (fib-iter a
                   b
                   ; Copied from SICP except for these 2 lines
                   ; Compute p' = p^2 + q^2
                   (+ (square p) (square q))
                   ; Computer q' = 2pq + q^2
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else
         (fib-iter (+ (* b q) (* a q) (* a p))
                   (+ (* b p) (* a q))
                   p
                   q
                   (- count 1)))))
 (fib-iter 1 0 0 1 n))
