; Exercise 2.81

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

; Begin copied from SICP

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

  ; Specific to exercise 2.81
  (put 'exp
       '(scheme-number scheme-number)
       (lambda (x y)
         (tag (expt x y))))

  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2
                       (get-coercion type1
                                     type2))
                      (t2->t1
                       (get-coercion type2
                                     type1)))
                  (cond (t1->t2
                         (apply-generic
                          op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic
                          op a1 (t2->t1 a2)))
                        (else
                         (error
                          "No method for
                           these types"
                          (list
                           op
                           type-tags))))))
              (error
               "No method for these types"
               (list op type-tags)))))))

(define (scheme-number->complex n)
  (make-complex-from-real-imag
   (contents n) 0))

(put-coercion 'scheme-number 'complex
              scheme-number->complex)

; Specific to Exercise 2.81
(define (exp x y)
  (apply-generic 'exp x y))

; End copied from SICP

(install-scheme-number-package)
(install-complex-package)

; Rectangular additions hacked into complex package
(put 'make-from-real-imag 'rectangular
     (lambda (x y) (attach-tag 'rectangular (cons x y))))
(put 'real-part '(rectangular) (lambda (c) (car c)))
(put 'imag-part '(rectangular) (lambda (c) (cdr c)))

(newline)
(display "Without Louis' coercions:")

(newline)
(display "2^3: ")
(display (exp (make-scheme-number 2) (make-scheme-number 3)))

(define a (make-complex-from-real-imag 2 0))
(define b (make-complex-from-real-imag 3 0))
; Produces an error when it can't find an implementation of exp for complex
; arguments
;(newline)
;(display "(2+0i)^(3+0i): ")
;(display (exp a b))

; Louis' coercions, copied from SICP
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)

(put-coercion 'complex 'complex
              complex->complex)

(newline)
(display "With Louis' coercions:")

(newline)
(display "2^3: ")
(display (exp (make-scheme-number 2) (make-scheme-number 3)))

; Recurses infinitely due to never finding an implementation of exp for complex
; numbers but repeatedly finding a conversion from complex to complex and trying
; again with its result.
;(newline)
;(display "(2+0i)^(3+0i): ")
;(display (exp a b))

(newline)
(display "SN->C coercion: ")
(display (get-coercion 'scheme-number 'complex))

(newline)
(display "SN->SN coercion: ")
(display (get-coercion 'scheme-number 'scheme-number))

(newline)
(display "C->C coercion: ")
(display (get-coercion 'complex 'complex))

; Adapted from SICP for Exercise 2.81, part 3
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags)))
                (if (not (equal? type1 type2))
                    (let ((t1->t2
                            (get-coercion type1
                                          type2))
                          (t2->t1
                            (get-coercion type2
                                          type1))
                          (a1 (car args))
                          (a2 (cadr args)))
                      (cond (t1->t2
                              (apply-generic
                                op (t1->t2 a1) a2))
                            (t2->t1
                              (apply-generic
                                op a1 (t2->t1 a2)))
                            (else
                              (error
                                "No method for
                                these types"
                                (list
                                  op
                                  type-tags)))))
                    (error
                      "No method for these types"
                      (list op type-tags))))
                (error
                  "No method for these types"
                  (list op type-tags)))))))

(newline)
(display "2^3: ")
(display (exp (make-scheme-number 2) (make-scheme-number 3)))

; Now merely produces an error, despite the fact that Louis' coercions are
; installed.
(newline)
(display "(2+0i)^(3+0i): ")
(display (exp a b))
