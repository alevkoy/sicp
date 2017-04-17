; Exercise 2.40
; enumerate-interval, flatmap, make-pair-sum, smallest-divisor, prime?, and
; prime-sum? adapted from SICP.

(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low
              (enumerate-interval (1+ low)
                                  high))))

(define (flatmap proc seq)
    (fold-right append '() (map proc seq)))

(define (unique-pairs n)
    (flatmap (lambda (i)
                 (map (lambda (j)
                          (list i j))
                      (enumerate-interval 1 (-1+ i))))
             (enumerate-interval 1 n)))

(newline)
(display "unique-pairs: ")
(display (unique-pairs 4))

(define (make-pair-sum pair)
    (list (car pair)
          (cadr pair)
          (+ (car pair) (cadr pair))))

(define (smallest-divisor n)
    (define (divides? a b)
        (= (remainder b a) 0))

    (define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n)
               n)
              ((divides? test-divisor n)
               test-divisor)
              (else (find-divisor n
                                  (+ test-divisor 1)))))

    (find-divisor n 2))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum?
                 (unique-pairs n))))

(newline)
(display "prime-sum-pairs: ")
(display (prime-sum-pairs 6))

; Exercise 2.41

(define (unique-triples n)
    (flatmap (lambda (i)
                 (flatmap (lambda (j)
                              (map (lambda (k)
                                       (list i j k))
                                   (enumerate-interval 1 (-1+ j))))
                          (enumerate-interval 1 (-1+ i))))
             (enumerate-interval 1 n)))

(define (make-triple-sum triple)
    (append triple
            (list (fold-left + 0 triple))))

(define (s-sum-triples n s)
    (map make-triple-sum
         (filter (lambda (triple)
                     (= (fold-left + 0 triple)
                        s))
                 (unique-triples n))))

(newline)
(display "s-sum-triples: ")
(display (s-sum-triples 6 8))
