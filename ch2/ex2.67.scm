; Exercise 2.67

; Begin copied from SICP

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch
                (car bits)
                current-branch)))
          (if (leaf? next-branch)
              (cons
               (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits)
                        next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit:
               CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
         (cons x set))
        (else
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree
    (make-leaf 'A 4)
    (make-code-tree
      (make-leaf 'B 2)
      (make-code-tree
        (make-leaf 'D 1)
        (make-leaf 'C 1)))))

(define sample-message
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; End copied from SICP

(newline)
(display "Message: ")
(display sample-message)

(define decoded (decode sample-message sample-tree))

(newline)
(display "Decoded: ")
(display decoded)

; Exercise 2.68
; encode copied from SICP.

(define (encode message tree)
  (if (null? message)
      '()
      (append
       (encode-symbol (car message)
                      tree)
       (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (choose-branch symbol tree)
    (let ((left (left-branch tree))
          (right (right-branch tree)))
      (cond ((memq symbol (symbols left)) (cons 0 left))
            ((memq symbol (symbols right)) (cons 1 right))
            (else (error "symbol not in tree:" symbol)))))

  (define (encode-bit symbol current-branch)
    (if (leaf? current-branch)
        '()
        (let ((next-bit-branch (choose-branch symbol current-branch)))
          (cons (car next-bit-branch)
                (encode-bit symbol (cdr next-bit-branch))))))

  (encode-bit symbol tree))

(newline)
(display "Encoded: ")
(display (encode decoded sample-tree))

; Exercise 2.69
; generate-huffman-tree copied from SICP.

(define (generate-huffman-tree pairs)
  (successive-merge
    (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (define (iterate tree leaf-set)
    (cond ((null? leaf-set) tree)
          ((null? tree) (iterate (car leaf-set) (cdr leaf-set)))
          (else (iterate (make-code-tree (car leaf-set) tree)
                         (cdr leaf-set)))))

  (iterate '() leaf-set))

(define pairs (list (list 'A 4) (list 'D 1) (list 'B 2) (list 'C 1)))
(define sample-tree2 (generate-huffman-tree pairs))

(newline)
(display "Sample tree: ")
(display sample-tree)
(newline)
(display "Constructed tree: ")
(display sample-tree2)

; Exercise 2.70

(define message '(Get a job
                  Sha na na na na na na na na
                  Get a job
                  Sha na na na na na na na na
                  Wah yip yip yip yip
                  yip yip yip yip yip
                  Sha boom))

(define tree (generate-huffman-tree '((A 2) (NA 16)
                                      (BOOM 1) (SHA 3)
                                      (GET 2) (YIP 9)
                                      (JOB 2) (WAH 1))))

(newline)
(display "Encoded message: ")
(define encoded (encode message tree))
(display encoded)
(newline)
(display "Bits: ")
(display (length encoded))
