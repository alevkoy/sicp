; mit-scheme does not support the picture language described in SICP.
; Consequently, it will not be possible to run this code and get the result
; described in the text. For the sake of making sure my code parses correctly,
; I have implemented the picture primitives below and beside in terms of lists
; of numbers.

(define (below first second)
    (list first second))

(define (beside first second)
    (list first second))

; Exericse 2.44

(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter
                                 (- n 1))))
            (below painter
                   (beside smaller smaller)))))

; Exercise 2.45

(define (split op-bigger op-smaller)
    (lambda (painter n)
        (if (= n 0)
            painter
            (let ((smaller ((split op-bigger op-smaller) painter
                                                         (- n 1))))
                (op-bigger painter
                           (op-smaller smaller smaller))))))

(define right-split (split beside below))
(define up-split (split below beside))

; Exercise 2.46

(define (make-vect x y)
    (cons x y))

(define (xcor-vect v)
    (car v))

(define (ycor-vect v)
    (cdr v))

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1)
                  (xcor-vect v2))
               (+ (ycor-vect v1)
                  (ycor-vect v2))))

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1)
                  (xcor-vect v2))
               (- (ycor-vect v1)
                  (ycor-vect v2))))

(define (scale-vect s v)
    (make-vect (* s (xcor-vect v))
               (* s (ycor-vect v))))

(define v1 (make-vect 5 3))
(define v2 (make-vect 2 4))

(newline)
(display (add-vect v1 v2))
(newline)
(display (sub-vect v1 v2))
(newline)
(display (scale-vect 7 v1))

; Exercise 2.47
; Both implementations of make-frame adapted from SICP.

; Implementation 1
(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

(define (origin-frame frame)
    (car frame))

(define (edge1-frame frame)
    (cadr frame))

(define (edge2-frame frame)
    (caddr frame))

(define origin (make-vect 0 0))
(define edge1 (make-vect -1 5))
(define edge2 (make-vect 5 1))
(define f1 (make-frame origin edge1 edge2))

(newline)
(display "Implementation 1")
(newline)
(display (origin-frame f1))
(newline)
(display (edge1-frame f1))
(newline)
(display (edge2-frame f1))

; Implementation 2
(define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

; origin-frame and edge1-frame don't change.
(define (origin-frame frame)
    (car frame))

(define (edge1-frame frame)
    (cadr frame))

(define (edge2-frame frame)
    (cddr frame))

(define f2 (make-frame origin edge1 edge2))

(newline)
(display "Implementation 2")
(newline)
(display (origin-frame f2))
(newline)
(display (edge1-frame f2))
(newline)
(display (edge2-frame f2))

; Exercise 2.48

(define (make-segment start end)
    (cons start end))

(define (start-segment s)
    (car s))

(define (end-segment s)
    (cdr s))

(define s1 (make-segment v1 v2))

(newline)
(display (start-segment s1))
(newline)
(display (end-segment s1))

; Exericse 2.49
; frame-coord-map and segments->painter adapted from SICP.

(define (frame-coord-map frame)
    (lambda (v)
        (add-vect (origin-frame frame)
                  (add-vect
                        (scale-vect (xcor-vect v)
                                    (edge1-frame frame))
                        (scale-vect (ycor-vect v)
                                    (edge2-frame frame))))))

(define (segments->painter segment-list)
    (lambda (frame)
        (for-each (lambda (segment)
                      (draw-line ((frame-coord-map frame)
                                  (start-segment segment))
                                 ((frame-coord-map frame)
                                  (end-segment segment))))
                  segment-list)))

; Obviously, this does not actually draw the specified lines on the screen, but
; it does allow the below code to be interpreted without undefined-symbol
; errors, and it permits some basic checking of the results.
(define (draw-line start end)
    (newline)
    (display start)
    (display " -> ")
    (display end))

(define outline
    (segments->painter (list (make-segment (make-vect 0 0)
                                           (make-vect 0 1))
                             (make-segment (make-vect 0 1)
                                           (make-vect 1 1))
                             (make-segment (make-vect 1 1)
                                           (make-vect 1 0))
                             (make-segment (make-vect 1 0)
                                           (make-vect 0 0)))))

(define x
    (segments->painter (list (make-segment (make-vect 0 0)
                                           (make-vect 1 1))
                             (make-segment (make-vect 0 1)
                                           (make-vect 1 0)))))

(define diamond
    (segments->painter (list (make-segment (make-vect .5 0)
                                           (make-vect 1 .5))
                             (make-segment (make-vect 1 .5)
                                           (make-vect .5 1))
                             (make-segment (make-vect .5 1)
                                           (make-vect 0 .5))
                             (make-segment (make-vect 0 .5)
                                           (make-vect .5 0)))))

; I do not define the wave painter, due to the tedium and pointlessness of that
; task.

; Exercise 2.50
; transform-painter adapted from SICP.

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-coord-map frame)))
            (let ((new-origin (m origin)))
                (painter (make-frame new-origin
                                     (sub-vect (m corner1)
                                               new-origin)
                                     (sub-vect (m corner2)
                                               new-origin)))))))

(define (flip-horiz painter)
    (transform-painter painter
                       (make-vect 1 0)
                       (make-vect 0 0)
                       (make-vect 1 1)))

(define (rotate180 painter)
    (transform-painter painter
                       (make-vect 1 1)
                       (make-vect 0 1)
                       (make-vect 1 0)))

(define (rotate270 painter)
    (transform-painter painter
                       (make-vect 0 1)
                       (make-vect 0 0)
                       (make-vect 1 1)))

; A line from the center to the upper right corner, from which all of the
; transformations should be evident.
(define test-painter
    (segments->painter (list (make-segment (make-vect .5 .5)
                                           (make-vect 1 1)))))
(define unit-frame (make-frame (make-vect 0 0)
                               (make-vect 1 0)
                               (make-vect 0 1)))

(newline)
(display "Test: ")
(test-painter unit-frame)
(newline)
(display "Flip horizontal: ")
((flip-horiz test-painter) unit-frame)
(newline)
(display "Rotate 180: ")
((rotate180 test-painter) unit-frame)
(newline)
(display "Rotate 270 ")
((rotate270 test-painter) unit-frame)
