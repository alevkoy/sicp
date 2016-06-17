; Exercise 1.23
; Smallest divisor, prime, and timed-prime-test adapted from SICP
(define (smallest-divisor n)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  ; next is new in this solution
  (define (next n)
    (if (= n 2)
        3
        (+ n 2)))
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
