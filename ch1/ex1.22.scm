; Exercise 1.22
; smallest-divisor, prime, and timed-prime-test adapted from SICP
(define (smallest-divisor n)
  (define (divides? a b)
    (= (remainder b a) 0))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n)
           n)
          ((divides? test-divisor n)
           test-divisor)
          (else (find-divisor
                 n
                 (+ test-divisor 1)))))

  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime n (- (runtime) start-time))))

  ; Only produce output when a prime is found, not for every number tested
  (define (report-prime n elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))

  (start-prime-test n (runtime)))

(define (search-for-primes beginning end)
  (define (iterate beginning end)
    (timed-prime-test beginning)
    (search-for-primes (1+ beginning) end))

  (if (< beginning end)
      (iterate beginning end)))
