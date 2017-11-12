; Exercise 2.73

; Begin copied from SICP.

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp)
           (if (same-variable? exp var)
               1
               0))
         (else ((get 'deriv (operator exp))
                (operands exp)
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

; End copied from SICP.

; Begin copied from Stack Overflow
; Question: https://stackoverflow.com/q/5499005
; by KnowsLittle: https://stackoverflow.com/users/441693/knowslittle
; Answer: https://stackoverflow.com/a/19114031
; by maxbublis: https://stackoverflow.com/users/2592125/maxbublis

(define *op-table* (make-hash-table))

(define (put op type proc)
  (hash-table/put! *op-table* (list op type) proc))

(define (get op type)
  (hash-table/get *op-table* (list op type) #f))

; End copied from Stack Overflow

(define (install-sum-deriv-package)
  ; Internal procedures
  ; addend and augend are not quite what would be expected from make-sum
  ; because deriv is called with just the list of operands, absent the
  ; operator.
  (define (addend s) (car s))

  (define (augend s) (cadr s))

  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  ; External interface
  (define (tag x)
    (attach-tag '+ x))

  (put 'deriv '+ deriv-sum)

  'done)

(define (install-product-deriv-package)
  ; Internal procedures
  ; multiplier and multiplicand are similar to addend and augend.
  (define (multiplier p) (car p))

  (define (multiplicand p) (cadr p))

  (define (deriv-product exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var)
                            (multiplicand exp))))

  ; External interface
  (define (tag x)
    (attach-tag '* x))

  (put 'deriv '* deriv-product)

  'done)

(install-sum-deriv-package)
(install-product-deriv-package)

(newline)
(display "d(3x)/dx = ")
(display (deriv '(* 3 x) 'x))

(newline)
(display "d(3x + x + 17)/dx = ")
(display (deriv '(+ (+ (* 3 x) x) 17) 'x))

; exponentiation? and make-exponentiation copied from solution to Exercise 2.56.
(define (exponentiation? x)
  (and (pair? x)
       (eq? (car x) '**)))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list '** b e))))

(define (install-exponentiation-deriv-package)
  ; Internal procedures
  (define (base e)
    (car e))

  (define (exponent e)
    (cadr e))

  (define (deriv-exponentiation exp var)
    (let ((n (exponent exp))
          (b (base exp)))
      (make-product n
                    (make-product
                      (make-exponentiation b
                                          (make-sum n -1))
                      (deriv b var)))))

  ; External interface
  (define (tag x)
    (attach-tag '** x))

  (put 'deriv '** deriv-exponentiation)

  'done)

(install-exponentiation-deriv-package)

(newline)
(display "d(3x**2 + x)/dx = ")
(display (deriv '(+ (* 3 (** x 2)) x) 'x))
