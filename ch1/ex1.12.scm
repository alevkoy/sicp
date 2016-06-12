; Exercise 1.12
(define (pascal row column)
 ; Return 0 when nonexistent location is specified to
 ; make computations at edges simpler
 (cond ((> column row) 0)
       ((< column 1) 0)
       ((< row 1) 0)
       ((= row 1) 1)
       (else (+ (pascal (-1+ row) (-1+ column))
                (pascal (-1+ row) column)))))
