; Exercise 1.44

(define (compose f g)
    (lambda (x) (f (g x))))

(define (repeated f n)
    (define (iter count result)
        (if (<= count 1)
            result
            (iter (-1+ count)
                  (compose f result))))

    (iter n f))

(define (smooth f)
    (let ((dx 0.0001))
        (lambda (x) (/ (+ (f (+ x dx))
                          (f x)
                          (f (- x dx)))
                       3))))

(define (nfold-smooth f n)
    ((repeated smooth n) f))

; I don't have a great idea for how to test this, because I'm not sure what a
; smoothed function is supposed to look like. At minimum, it seems like it
; should be approximately equal to any continuous input function.
(newline)
(display ((smooth square) 5))

(newline)
(display ((nfold-smooth square 3) 5))
