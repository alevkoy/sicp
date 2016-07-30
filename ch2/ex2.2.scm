; Exercise 2.2
; average and print-point adapted from SICP.

(define (average a b)
    (/ (+ a b) 2))

(define (print-point p)
    (newline)
	(display "(")
	(display (x-point p))
	(display ",")
	(display (y-point p))
	(display ")"))

(define (print-segment s)
    (newline)
	(display "(")
	(display (x-point (start-point s)))
	(display ",")
	(display (y-point (start-point s)))
	(display ") -> (")
	(display (x-point (end-point s)))
	(display ",")
	(display (y-point (end-point s)))
	(display ")"))

(define (make-segment start end)
    (cons start end))

(define (start-point segment)
    (car segment))

(define (end-point segment)
    (cdr segment))

(define (make-point x y)
    (cons x y))

(define (x-point point)
    (car point))

(define (y-point point)
    (cdr point))

(define (midpoint-segment s)
    (let ((start (start-point s))
          (end (end-point s)))
        (let ((start-x (x-point start))
              (end-x (x-point end))
              (start-y (y-point start))
              (end-y (y-point end)))
            (make-point (average start-x end-x)
                        (average start-y end-y)))))

(define s (make-segment (make-point 0 0)
                        (make-point 10 6)))
(print-segment s)
(print-point (midpoint-segment s))
