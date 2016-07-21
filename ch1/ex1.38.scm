; Exercise 1.38

(define (cont-frac n d k)
    (define (iter k result)
        (if (= k 0)
            result
            (iter (-1+ k)
                  (/ (n k)
                     (+ (d k) result)))))

    (iter k 0))

(define (e k)
    (+ (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                      (if (= (remainder i 3) 2)
                          (* (/ 2 3) (+ i 1))
                          1))
                  k)
       2))

(newline)
(display "e = ")
(display (e 10))
