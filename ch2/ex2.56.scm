; Exercise 2.56
; variable through multiplicand adapted from SICP.

; Begin copied from SICP
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))
; End copied from SICP

(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (base e)
  (cadr e))

(define (exponent e)
  (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

; Adapted from SICP; extended with exponentiation? clause.
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (deriv (multiplier exp) var)
           (multiplicand exp))))
        ; New code here
        ((exponentiation? exp)
         (let ((n (exponent exp))
               (b (base exp)))
           (make-product n
                         (make-product
                           (make-exponentiation b
                                                (make-sum n -1))
                           (deriv b var)))))
        (else (error "unknown expression type: DERIV" exp))))

; Just a sanity check for my copy-pasting
(newline)
(display "d/dx 5x+2x = ")
(display (deriv '(+ (* 5 x) (* 2 x)) 'x))

; Exponentiation
(newline)
(display "d/dx x^5 = ")
(display (deriv '(** x 5) 'x))

(newline)
(display "d/dx (xy)^5 = ")
(display (deriv '(** (* x y) 5) 'x))
