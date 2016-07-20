; Exercise 1.37

; Iterative
(define (cont-frac n d k)
    (define (iter k result)
        (if (= k 0)
            result
            (iter (-1+ k)
                  (/ (n k)
                     (+ (d k) result)))))

    (iter k 0))

(define (inverse-golden-ratio k)
    (cont-frac (lambda (i) 1.0)
               (lambda (i) 1.0)
               k))

(newline)
(display (inverse-golden-ratio 10))

; Recursive
(define (cont-frac n d k)
    (define (recurse k current)
        (if (= current k)
            (/ (n k) (d k))
            (/ (n k)
               (+ (d k)
                  (recurse k (1+ current))))))

    (recurse k 1))

(newline)
(display (inverse-golden-ratio 10))
