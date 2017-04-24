; Exericse 2.44
; mit-scheme does not support the picture language described in SICP.
; Consequently, it will not be possible to run this code and get the result
; described in the text. For the sake of making sure my code parses correctly,
; I have implemented the picture primitives below and beside in terms of lists
; of numbers.

(define (below first second)
    (list first second))

(define (beside first second)
    (list first second))

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
