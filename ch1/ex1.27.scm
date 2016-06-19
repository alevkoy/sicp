; Exercise 1.27
; expmod copied from SICP
(define (carmichael n)
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
  (define (iter a)
    (cond ((= a 0) #t)
          ; Apply Fermat test
          ((= (expmod a n n) a)
           ; Continue towards 0 if this value of a passed
           (iter (- a 1)))
          (else #f)))

  (iter (- n 1)))

(define (display-passed n)
  (newline)
  (display n)
  (display " passes Fermat test"))

(define (display-carmichael n)
  (if (carmichael n)
      (display-passed n)))

(display-carmichael  561)
(display-carmichael 1105)
(display-carmichael 1729)
(display-carmichael 2465)
(display-carmichael 2821)
(display-carmichael 6601)
