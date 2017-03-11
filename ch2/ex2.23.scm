; Exercise 2.23

(define (for-each proc elements)
    (define (recurse proc elements)
        (proc (car elements))
        (for-each proc (cdr elements)))

    (if (not (null? elements))
        (recurse proc elements)))

(for-each (lambda (x)
             (newline)
             (display x))
          (list 1 2 3 4))
