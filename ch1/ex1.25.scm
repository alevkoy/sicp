; Code for exercise 1.25
; All copied verbatim from SICP, except for report-prime, which has been modified
; to only produce output when primes are found
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))


(define (square x) (* x x))

(define (fast-prime? n times)
  (define (fermat-test n)
    ; See if candidate n passes the Fermat test for test value a
    (define (try-it a)
      (= (expmod a n n) a))

    ; Perform the Fermat test on n with a random value less than n
    (try-it (+ 1 (random (- n 1)))))

  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (if (fast-prime? n 5)
        (report-prime n (- (runtime) start-time))))

  ; Only produce output when a prime is found, not for every number tested
  (define (report-prime n elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))

  (start-prime-test n (runtime)))

(timed-prime-test    1223)
