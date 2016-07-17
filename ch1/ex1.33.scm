; Exercise 1.33
; smallest-divisor, prime? adapted from SICP.

(define (filtered-accumulate combiner null-value filter term a next b)
    ; Since this problem does not require recursive and iterative versions,
    ; I will just do an iterative version
    (define (iter term a next b result)
        (define filtered-term (if (filter a)
                                  (term a)
                                  null-value))

        (if (> a b)
            result
            (iter term
                  (next a)
                  next
                  b
                  (combiner filtered-term
                            result))))

    (iter term a next b null-value))

(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))

  (define (divides? a b)
    (= (remainder b a) 0))

  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))

  (find-divisor n 2))

(define (prime? n)
    (and (> n 1)
         (= n (smallest-divisor n))))

; Sum of the squares of the prime numbers in [a,b]
(define (sum-squared-primes a b)
    (define (term x) (square x))

    (define (next x) (1+ x))

    (filtered-accumulate +
                         0
                         prime?
                         term
                         a
                         next
                         b))

(define (product-relative-primes n)
    (define (rel-prime? x)
        (= (gcd x n) 1))

    (define (term x) x)

    (filtered-accumulate *
                         1
                         rel-prime?
                         term
                         1
                         1+
                         ; n isn't part of the product
                         (- n 1)))

(newline)
(display "Sum of squared primes from 1 to 10: ")
(display (sum-squared-primes 1 10))

(newline)
(display "Product of positive integers less than and relatively prime to 12: ")
(display (product-relative-primes 12))
