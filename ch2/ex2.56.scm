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

(newline)
(display "Exercise 2.56")

; Exponentiation
(newline)
(display "d/dx x^5 = ")
(display (deriv '(** x 5) 'x))

(newline)
(display "d/dx (xy)^5 = ")
(display (deriv '(** (* x y) 5) 'x))

; Exercise 2.57

; Change the representation of sums to a list of terms, prefixed with '+
; make-sum and addend stay the same.

(define (augend s)
  (if (null? (cdddr s))
      ; Only 2 terms
      (caddr s)
      ; More than 2 terms
      (cons '+ (cddr s))))

(define (multiplicand p)
  (if (null? (cdddr p))
      ; Only 2 terms
      (caddr p)
      ; More than 2 terms
      (cons '* (cddr p))))

(newline)
(display "Exercise 2.57")

(newline)
(display "d/dx 5x+2x = ")
(display (deriv '(+ (* 5 x) (* 2 x)) 'x))

(newline)
(display "d/dx xy(x+3) = ")
(display (deriv '(* x y (+ x 3)) 'x))

(newline)
(display "d/dx x^4+2x^3+3x^4 = ")
(display (deriv '(+ (** x 4) (* 2 (** x 3)) (* 3 (** x 4))) 'x))

; Exercise 2.58

; Part 1 - infix operators, fully parenthesized
; augend and multiplicand copied from SICP. make-sum, sum?, addend, augend,
; make-product, product?, multiplier, and muliplicand based on original versions
; in SICP.

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x)
       (pair? (cdr x))
       (eq? (cadr x) '+)))

(define (addend s)
  (car s))

(define (augend s)
  (caddr s))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (pair? x)
       (pair? (cdr x))
       (eq? (cadr x) '*)))

(define (multiplier p)
  (car p))

(define (multiplicand p)
  (caddr p))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list b '** e))))

(define (exponentiation? x)
  (and (pair? x)
       (pair? (cdr x))
       (eq? (cadr x) '**)))

(define (base e)
  (car e))

(define (exponent e)
  (caddr e))

(newline)
(display "Exercise 2.58, part 1")

(newline)
(display "d/dx 2x^2 + 3 = ")
(display (deriv '((2 * (x ** 2)) + 3) 'x))

; Part 2 - infix operators, not fully parenthesized

; A poor-man's precedence table. e must be a list.
(define (lowest-precedence-op e)
  (cond ((memq '+ e) '+)
        ((memq '* e) '*)
        ((memq '** e) '**)))

; If l is a list with one element, just return that element. This
; is necessary, because deriv can't handle expressions that are
; singleton lists.
(define (singleton-to-bare l)
  (if (and (pair? l)
           (= (length l) 1))
      (car l)
      l))

; Get the first operand of the first instance of operator in the
; expression e. e is assumed not to have any operators of higher
; precedence than operator.
(define (first-operand e operator)
  (define (recurse e)
    (if (eq? (car e) operator)
        '()
        (cons (car e)
              (recurse (cdr e)))))
  (singleton-to-bare (recurse e)))

; Get the second operand of the first instance of operator in e.
(define (second-operand e operator)
  (singleton-to-bare (cdr (memq operator e))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (eq? (lowest-precedence-op x) '+))

(define (addend s)
  (first-operand s '+))

(define (augend s)
  (second-operand s '+))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (eq? (lowest-precedence-op x) '*))

(define (multiplier p)
  (first-operand p '*))

(define (multiplicand p)
  (second-operand p '*))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        (else (list b '** e))))

(define (exponentiation? x)
  (eq? (lowest-precedence-op x) '**))

(define (base e)
  (first-operand e '**))

(define (exponent e)
  (second-operand e '**))

(newline)
(display "Exercise 2.58, part 2")

(newline)
(display "d/dx x^3^2 = ")
(display (deriv '(x ** 3 ** 2) 'x))

(newline)
(display "d/dx 2*x^3 = ")
(display (deriv '(8 * x ** 3) 'x))

(newline)
(display "d/dx 2*x*x^2 = ")
(display (deriv '(2 * x * x ** 2) 'x))

(newline)
(display "d/dx 2x+3 = ")
(display (deriv '(2 * x + 3) 'x))

(newline)
(display "d/dx 5x^2 + 3x + 5x = ")
(display (deriv '(5 * x ** 2 + 3 * x + (5 + 13) * x) 'x))
