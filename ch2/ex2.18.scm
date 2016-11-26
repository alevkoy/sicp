; Exercise 2.18

(define (reverse list1)
    (define (reverse-iter from to)
        (if (null? from)
            to
            (reverse-iter (cdr from)
                     (cons (car from) to))))
    (reverse-iter list1 '()))
