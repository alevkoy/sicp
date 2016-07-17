; Exercise 1.28
; fast-prime? and timed-prime-test adapted from SICP.
; timed-prime-test has been modified to print less output and to use
; fast-prime?. fast-prime? has been modified to use rm-test.
; expmod has been modified to identify non-trivial roots of 1 modulo n and
; return immediately if it finds any.
; timed-prime-test has been modified to perform at most enough tests to ensure
; that the Miller-Rabin test does not produce any false positives.

(define (fast-prime? n times)
  (define (rm-test n)
    ; Raise base to the power exp modulo m; return 0
    ; if a nontrivial root of exp is identified
    (define (expmod base exp m)
      ; Split the return value computation into steps
      ; to allow the inspection of the remainder in
      ; the squaring case without computing it twice.
      ; This ends up checking the value of exp twice,
      ; but the alternative would have been using the
      ; let keyword, which isn't introduced until
      ; section 1.3.
      (define recurse-value
        (cond ((= exp 0) 1)
              ((even? exp)
               (expmod base (/ exp 2) m))
              (else
               (expmod base (- exp 1) m))))

      (define dividend
         (cond ((= exp 0) 1)
               ((even? exp)
                (square recurse-value))
               (else (* base recurse-value))))

      (define rem-value (remainder dividend m))

      ; Non-trivial roots of 1 modulo n are those that
      ; are not equal to 1 or n-1; they can only be
      ; found when dividend was computed by squaring,
      ; which is to say when exp is even. If one is
      ; found, return 0 to cause try-it to fail.
      (cond ((and (even? exp)
                  (= rem-value 1)
                  (not (or (= recurse-value 1)
                           (= recurse-value (- n 1)))))
             0)
            (else (remainder dividend m))))

    ; See if candidate n passes the Miller-Rabin test
    ; for test value a
    (define (try-it a)
      (= (expmod a (- n 1) n)
         1))

    ; Perform the Miller-Rabin test on n with a random
    ; value less than n
    (try-it (+ 1 (random (- n 1)))))

  (cond ((= times 0) #t)
        ((rm-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    ; The Miller-Rabin test will find a non-trivial
    ; root of 1 modulo n for at least half of the
    ; numbers a < n if n is not prime. Therefore, we
    ; need to test at most half of such a (or half + 1,
    ; since this only works for odd n). For large
    ; numbers, it is not practical to test half of the
    ; numbers below n, so test at most max-tries.
    (define at-least-half (+ (/ n 2) 1))

    (define max-tries 500)

    (define tries (if (and (odd? n)
                           (< at-least-half max-tries))
                      at-least-half
                      max-tries))

    (if (fast-prime? n tries)
        (report-prime n (- (runtime) start-time))
        (report-not-prime n (- (runtime) start-time))))

  (define (report-prime n elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))

  (define (report-not-prime n elapsed-time)
    (newline)
    (display n)
    (display " XXX ")
    (display elapsed-time))

  (start-prime-test n (runtime)))

; Prime numbers
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

; Carmichael numbers
(timed-prime-test   561)
(timed-prime-test  1105)
(timed-prime-test  1729)
(timed-prime-test  2465)
(timed-prime-test  2821)
(timed-prime-test  6601)
(timed-prime-test  8911)
(timed-prime-test 10585)
(timed-prime-test 15841)
(timed-prime-test 29341)
(timed-prime-test 41041)
(timed-prime-test 46657)
(timed-prime-test 52633)
(timed-prime-test 62745)
(timed-prime-test 63973)
(timed-prime-test 75361)
