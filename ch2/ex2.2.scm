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

; Exercise 2.3
; square adapted from SICP.

; Implementation #1: 3 points (The 4th point could be derived if necessary.)
(define (make-rectangle p1 p2 p3)
    (cons p1 (cons p2 p3)))

(define (p1-rectangle r)
    (car r))

(define (p2-rectangle r)
    (car (cdr r)))

(define (p3-rectangle r)
    (car (cdr r)))

(define (square x) (* x x))

(define (length-segment s)
    ; Use Pythagorean Theorem
    (let ((x-dist (- (x-point (end-point s))
                     (x-point (start-point s))))
          (y-dist (- (y-point (end-point s))
                     (y-point (start-point s)))))
        (sqrt (+ (square x-dist) (square y-dist)))))

; Arbitrarily, the p1->p2 distance is height
(define (height-rectangle r)
    (length-segment (make-segment (p1-rectangle r)
                                  (p2-rectangle r))))

; Since p1->p2 is height, p2->p3 must be width
(define (width-rectangle r)
    (length-segment (make-segment (p1-rectangle r)
                                  (p2-rectangle r))))

(define (area-rectangle r)
    (* (width-rectangle r) (height-rectangle r)))

(define (perimeter-rectangle r)
    (* (+ (width-rectangle r) (height-rectangle r))))

; A rectangle to use for testing
(define r (make-rectangle (make-point 0 1)
                          (make-point 2 3)
                          (make-point 4 1)))

(define (test-display r)
    (newline)
    (display "Area: ")
    (display (area-rectangle r))
    (newline)
    (display "Perimeter: ")
    (display (perimeter-rectangle r)))

(test-display r)

; Implementation #2: 2 segments
; s1 and s2 are intersecting line segments in a rectangle. They are assumed to
; share a point.
(define (make-rectangle s1 s2)
    (cons s1 s2))

(define (s1-rectangle r)
    (car r))

(define (s2-rectangle r)
    (cdr r))

(define (height-rectangle r)
    (length-segment (s1-rectangle r)))

(define (width-rectangle r)
    (length-segment (s2-rectangle r)))

(define r (make-rectangle (make-segment (make-point 0 1)
                                        (make-point 2 3))
                          (make-segment (make-point 2 3)
                                        (make-point 4 1))))

(test-display r)
