; Exercise 1.24
; fast-prime? and timed-prime-test adapted from SICP. timed-prime test has been
; modified to only print primes and to use fast-prime?
(define (fast-prime? n times)
  (define (fermat-test n)
    ; Raise base to the power exp modulo m
    (define (expmod base exp m)
      (cond ((= exp 0) 1)
            ((even? exp)
             (remainder (square (expmod base (/ exp 2) m))
                        m))
            (else
             (remainder (* base (expmod base (- exp 1) m))
                        m))))

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
    (if (fast-prime? n 5000)
        (report-prime n (- (runtime) start-time))))

  ; Only produce output when a prime is found, not for every number tested
  (define (report-prime n elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))

  (start-prime-test n (runtime)))

(timed-prime-test    1000000007)
(timed-prime-test    1000000009)
(timed-prime-test    1000000021)
(timed-prime-test   10000000019)
(timed-prime-test   10000000033)
(timed-prime-test   10000000061)
(timed-prime-test  100000000003)
(timed-prime-test  100000000019)
(timed-prime-test  100000000057)
(timed-prime-test 1000000000039)
(timed-prime-test 1000000000061)
(timed-prime-test 1000000000063)
