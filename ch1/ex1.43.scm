; Exercise 1.43

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter count result)
        ; Doesn't make sense to repeat less than one time, but don't want
        ; to recurse forever if caller passes n=0
        (if (<= count 1)
            result
            (iter (-1+ count)
                  (compose f result))))

    (iter n f))

(newline)
(display ((repeated square 2) 5))
