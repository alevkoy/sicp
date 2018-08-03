; Exercise 2.87

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

; Begin adapted from Eli Bendersky's website

; Eli has the equivalence predicate here as 'equal. This may have been a
; transcription error on Eli's part or a change between MIT Scheme versions.
(define *coercion-table* (make-hash-table equal?))

; hash-table-put! and hash-table-get replaced with hash-table/put! and
; hash-table/get, respectively. Perhaps another transcription error.
(define (put-coercion type-from type-to proc)
  (hash-table/put!
    *coercion-table*
    (list type-from type-to)
    proc))

(define (get-coercion type-from type-to)
  (hash-table/get
    *coercion-table*
    (list type-from type-to)
    #f))

; End adapted from Eli Bendersky's website

; Begin adapted from SICP

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum:
              TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum:
              CONTENTS" datum)))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))

  ; Hacked-in support for public acess to numer and denom
  ; XXX: This will produce untyped results, which will not be suitable for use
  ; in generic operations.
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)

  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular
  ;; and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag
          'rectangular)
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar)
     r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a)
         (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (scheme-number->complex n)
  (make-complex-from-real-imag
   (contents n) 0))

(put-coercion 'scheme-number 'complex
              scheme-number->complex)

; End copied from SICP

; Begin code copied from previous solutions

(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

(define (apply-generic-error type-tags)
  (error "No method for these types"
         (list type-tags)))

(define (get-next-higher-type tag)
  (cond
    ((equal? tag 'scheme-number) 'rational)
    ((equal? tag 'rational) 'real)
    ((equal? tag 'real) 'complex)
    (else (error "Cannot raise type " tag))))

(define (raise n)
  (let ((from-type (type-tag n))
        (to-type (get-next-higher-type (type-tag n))))
    ((get-coercion from-type to-type) n)))

(define (get-type-depth tag)
  (define (get-higher-type-and-depth tag depth)
    (if (equal? tag 'complex)
      (cons '() depth)
      (get-higher-type-and-depth
        (get-next-higher-type tag)
        (1+ depth))))

  (cdr (get-higher-type-and-depth tag 0)))

(define (higher-type? tag1 tag2)
  (< (get-type-depth tag1)
     (get-type-depth tag2)))

(define (highest-type type-tags)
  (define (iterate type type-tags)
    (cond ((null? type-tags) type)
          ((higher-type? type (car type-tags))
           (iterate type (cdr type-tags)))
          (else (iterate (car type-tags) (cdr type-tags)))))

  ; Assumes type-tags is non-null
  (iterate (car type-tags) (cdr type-tags)))

(define (coerce-raise src target-type)
  (let ((src-type (type-tag src)))
    (cond ((higher-type? src-type target-type)
           (error "Can't raise type to a lower type: "
                  src-type target-type))
          ((equal? (type-tag src) target-type) src)
          (else (coerce-raise (raise src) target-type)))))

(define (all-same-type? type-tags)
  (if (null? type-tags)
    #t
    (every (lambda (tag)
             (equal? (car type-tags) tag))
           (cdr type-tags))))

; Raise all the arguments to the type of the highest argument and try
; again. If they are already all the same type, try raising them each
; to the next highest type.
(define (apply-raise op type-tags args)
  (let ((target-type
          (if (all-same-type? type-tags)
            (get-next-higher-type (car type-tags))
            (highest-type type-tags))))
    (apply apply-generic op (map (lambda (arg)
                                   (coerce-raise arg target-type))
                                 args))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (let ((result (apply proc (map contents args))))
            ; This is a pretty janky way to detect type-tagged results
            (if (pair? result)
                (drop result)
                result))
          (apply-raise op type-tags args)))))

; Rectangular additions hacked into complex package
(put 'make-from-real-imag 'rectangular
     (lambda (x y) (attach-tag 'rectangular (cons x y))))
(put 'real-part '(rectangular) (lambda (c) (car c)))
(put 'imag-part '(rectangular) (lambda (c) (cdr c)))

; Hacked in support for real
(define (make-real x)
  (attach-tag 'real x))

; Hacked in support for rational accessors
(define (numer-rational q)
  ((get 'numer 'rational) q))

(define (denom-rational q)
  ((get 'denom 'rational) q))

; Hacked in support for real
(define (make-real x)
  (attach-tag 'real x))

; Coercions suitable for raise
(put-coercion 'scheme-number 'rational
              (lambda (x)
                (make-rational (contents x)
                               1)))

(put-coercion 'rational 'real
              (lambda (x)
                (make-real (/ (numer-rational (contents x))
                              (denom-rational (contents x))))))

(put-coercion 'real 'complex
              (lambda (x)
                (make-complex-from-real-imag (contents x)
                                             0)))

(define (project num)
  (apply-generic 'project num))

; Projections suitable for drop
(put 'project '(complex)
     (lambda (c)
       (make-real (real-part c))))

(define (real->rational r)
  (define (iter n d iteration)
    (if (or (= iteration 100)
            (= (round n) n))
        (make-rational n d)
        (iter (* 10 n) (* 10 d) (1+ iteration))))

  (iter r 1 0))

(put 'project '(real)
     real->rational)

(put 'project '(rational)
     (lambda (q)
       (let ((val (/ (numer-rational q) (denom-rational q))))
         (make-scheme-number (round val)))))

(define (drop x)
  (if (equal? (type-tag x) 'scheme-number)
      x
      (let ((projection (project x)))
        (if (not (equ? (raise projection) x))
            x
            (drop projection)))))

; Generic accessors for complex-number components
(define (real-part c)
  (apply-generic 'real-part c))
(define (imag-part c)
  (apply-generic 'imag-part c))
(define (magnitude c)
  (apply-generic 'magnitude c))
(define (angle c)
  (apply-generic 'angle c))

; Implementations of accessors for 'complex to unwrap 'rectangular
; or 'polar representations
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

; Generic trigonometric operations
(define (sin-generic arg)
  (apply-generic 'sin arg))
(define (cos-generic arg)
  (apply-generic 'cos arg))
(define (sqrt-generic arg)
  (apply-generic 'sqrt arg))
(define (atan-generic arg)
  (apply-generic 'atan arg))

; Implementations for 'real will work for lower types
(put 'sin '(real) (lambda (r) (make-real (sin r))))
(put 'cos '(real) (lambda (r) (make-real (cos r))))
(put 'sqrt '(real) (lambda (r) (make-real (sqrt r))))
(put 'atan '(real) (lambda (r) (make-real (atan r))))

; Arithmetic operations on real numbers that expect untyped components
(put 'add '(real real)
     (lambda (z1 z2) (make-real (+ z1 z2))))
(put 'sub '(real real)
     (lambda (z1 z2) (make-real (- z1 z2))))
(put 'mul '(real real)
     (lambda (z1 z2) (make-real (* z1 z2))))
(put 'div '(real real)
     (lambda (z1 z2) (make-real (/ z1 z2))))

; Type-correct implementation of 'div for 'scheme-number
(put 'div '(scheme-number scheme-number)
     (lambda (n1 n2) (make-rational n1 n2)))

; Constructor and accessors for rectangular representations of complex
; numbers that expect typed components; real-part and imag-part defined
; above
(put 'magnitude '(rectangular)
     (lambda (c)
       (sqrt-generic (add (mul (car c) (car c))
                          (mul (cdr c) (cdr c))))))
(put 'angle '(rectangular)
     (lambda (c)
       (atan-generic (div (cdr c) (car c)))))

; Constructor and accessors for polar representations of complex
; numbers that expect typed components
(put 'make-from-mag-ang 'polar
     (lambda (x y) (attach-tag 'polar (cons x y))))
(put 'real-part '(polar)
     (lambda (c)
       (mul (car c) (cos-generic (cdr c)))))
(put 'imag-part '(polar)
     (lambda (c)
       (mul (car c) (sin-generic (cdr c)))))
(put 'magnitude '(polar) (lambda (c) (car c)))
(put 'angle '(polar) (lambda (c) (cdr c)))

; Arithmetic operations on complex numbers that expect typed components
(put 'add '(complex complex)
     (lambda (z1 z2)
       (make-complex-from-real-imag
         (add (real-part z1) (real-part z2))
         (add (imag-part z1) (imag-part z2)))))
(put 'sub '(complex complex)
     (lambda (z1 z2)
       (make-complex-from-real-imag
         (sub (real-part z1) (real-part z2))
         (sub (imag-part z1) (imag-part z2)))))
(put 'mul '(complex complex)
     (lambda (z1 z2)
       (make-complex-from-mag-ang
         (mul (magnitude z1) (magnitude z2))
         (add (angle z1) (angle z2)))))
(put 'div '(complex complex)
     (lambda (z1 z2)
       (make-complex-from-mag-ang
         (div (magnitude z1) (magnitude z2))
         (sub (angle z1) (angle z2)))))

; Coercions from and to complex numbers and related functions that expect
; complex numbers to have typed components
(put 'project '(complex) real-part)
(put-coercion 'real 'complex
              (lambda (x)
                ; x is already of type 'real
                (make-complex-from-real-imag x (make-real 0))))

; End code copied from previous solutions

; Begin adapted from SICP - setup for this problem

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ; TODO
  ;<procedures same-variable?
  ; and variable? from section 2.3.2>

  ;; representation of terms and term lists
  ; TODO
  ;<procedures adjoin-term ... coeff
  ;from text below>

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var:
               ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var:
               MUL-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2)
         (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms)
         (tag (make-poly var terms))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                   (adjoin-term
                     t1
                     (add-terms (rest-terms L1)
                                L2)))
                  ((< (order t1) (order t2))
                   (adjoin-term
                     t2
                     (add-terms
                       L1
                       (rest-terms L2))))
                  (else
                    (adjoin-term
                      (make-term
                        (order t1)
                        (add (coeff t1)
                             (coeff t2)))
                      (add-terms
                        (rest-terms L1)
                        (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms
        (mul-term-by-all-terms
          (first-term L1) L2)
        (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
          (make-term
            (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2)))
          (mul-term-by-all-terms
            t1
            (rest-terms L))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list)
  (null? term-list))
(define (make-term order coeff)
  (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

; End adapted from SICP

; For the sake of a reasonable implementation for poly, need to redefine this as
; a typical generic operation.
(define (=zero? x)
  (apply-generic '=zero? x))

(define (install-=zero?-package)
  (put '=zero? '(scheme-number) (lambda (n) (= n 0)))
  (put '=zero? '(rational) (lambda (q) (= (car q) 0)))
  (put '=zero? '(complex) (lambda (z) (and (=zero? (real-part z))
                                         (=zero? (imag-part z)))))
  (put '=zero? '(polynomial)
       (lambda (p)
         (let ((terms (cdr p)))
           (cond
             ((empty-termlist? terms) #t)
             ((not (=zero? (coeff (first-term terms)))) #f)
             (else (=zero? (make-polynomial (car p)
                                            (rest-terms terms))))))))

  'done)

(install-=zero?-package)
(install-polynomial-package)

; 3x + 2
(define non-zero-poly
  (make-polynomial 'x (list (make-term (make-scheme-number 1)
                                       (make-scheme-number 3))
                            (make-term (make-scheme-number 0)
                                       (make-scheme-number 2)))))
; 0
(define zero-poly
  (make-polynomial 'x (list (make-term (make-scheme-number 0)
                                       (make-scheme-number 0)))))
; 0x^7 + 0x^3 + 0x + 0
(define also-zero-poly
  (make-polynomial 'x (list (make-term (make-scheme-number 7)
                                       (make-scheme-number 0))
                            (make-term (make-scheme-number 3)
                                       (make-scheme-number 0))
                            (make-term (make-scheme-number 1)
                                       (make-scheme-number 0))
                            (make-term (make-scheme-number 0)
                                       (make-scheme-number 0)))))

(newline)
(display "3x + 2 == 0? ")
(display (=zero? non-zero-poly))

(newline)
(display "0x^0 == 0? ")
(display (=zero? zero-poly))

(newline)
(display "0x^7 + 0x^3 + 0x + 0x^0 == 0? ")
(display (=zero? also-zero-poly))
