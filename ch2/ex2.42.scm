; Exercise 2.42
; enumerate-interval, flatmap, and queens adapted from SICP.

(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low
              (enumerate-interval (1+ low)
                                  high))))

(define (flatmap proc seq)
    (fold-right append '() (map proc seq)))

(define (queens board-size)
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board)
            (filter (lambda (positions)
                        (safe? k positions))
                    (flatmap (lambda (rest-of-queens)
                                 (map (lambda (new-row)
                                          (adjoin-position new-row
                                                           k
                                                           rest-of-queens))
                                      (enumerate-interval 1 board-size)))
                             (queen-cols (- k 1))))))

    (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))

(define (safe? k positions)
    ; See if the current row is safe with respect to the previous row and
    ; proceed down the rows.
    (define (internal current previous-col previous)
        (if (null? previous)
            #t
            ; Can't be in the same row
            (and (not (= current (car previous)))
                 ; Can't be on the same diagonal
                 (not (= (- k previous-col)
                         (abs (- current (car previous)))))
                 ; And the previous rows must be safe
                 (internal current
                           (-1+ previous-col)
                           (cdr previous)))))

    (internal (car positions)
              (-1+ k)
              (cdr positions)))

(newline)
(display (queens 3))
