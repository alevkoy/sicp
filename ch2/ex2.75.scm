; Exercise 2.75

; Begin copied from SICP.

(define (real-part z)
  (apply-generic 'real-part z))
(define (imag-part z)
  (apply-generic 'imag-part z))
(define (magnitude z)
  (apply-generic 'magnitude z))
(define (angle z)
  (apply-generic 'angle z))

(define (apply-generic op arg) (arg op))

; End copied from SICP.

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))

  dispatch)

(define a (make-from-mag-ang 5 (atan 4 3)))


(newline)
(display "Magnitude: ")
(display (magnitude a))

(newline)
(display "Angle: ")
(display (angle a))

(newline)
(display "Real part: ")
(display (real-part a))

(newline)
(display "Imaginary part: ")
(display (imag-part a))
