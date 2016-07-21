; Exercise 1.39

(define (cont-frac n d k)
    (define (iter k result)
        (if (= k 0)
            result
            (iter (-1+ k)
                  (/ (n k)
                     (+ (d k) result)))))

    (iter k 0))

(define (tan-cf x k)
    (cont-frac (lambda (i)
                   (if (= i 1)
                       x
                       (- (square x))))
               (lambda (i) (- (* 2 i) 1))
               k))

(define pi 3.14159265359)
(newline)
(display "tan(pi/4) = ")
(display (tan-cf (/ pi 4) 100))
