; Exercise 2.29
; Both implementations of make-mobile and make-branch adapted from SICP.

(define (make-mobile left right)
    (list left right))

(define (make-branch length structure)
    (list length structure))

; Part 1
(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
    (cadr mobile))

(define (branch-length branch)
    (car branch))

(define (branch-structure branch)
    (cadr branch))

(define m (make-mobile "left" "right"))
(define b (make-branch "length" "structure"))

(newline)
(display "Left branch: ")
(display (left-branch m))
(newline)
(display "Right branch: ")
(display (right-branch m))
(newline)
(display "Length: ")
(display (branch-length b))
(newline)
(display "Structure: ")
(display (branch-structure b))

; Part 2
(define (total-weight mobile)
    ; A mobile's weight is the weight of its branches, and a branch's
    ; weight is the weight of its structure.
    (define (branch-weight branch)
        (total-weight (branch-structure branch)))

    (if (not (pair? mobile))
        ; A single weight is trivially a mobile.
        mobile
        ; Otherwise, the total weight is the weight of the branches
        ; combined.
        (+ (branch-weight (left-branch mobile))
           (branch-weight (right-branch mobile)))))

; Total weight is 2 + 4 + 13 + 1 = 20
(define m
    (make-mobile
        (make-branch 9
                     (make-mobile (make-branch 7 2)
                                  (make-branch 3 4)))
        (make-branch 3
                     (make-mobile (make-branch 8 13)
                                  (make-branch 2 1)))))

(newline)
(display "Total weight of m: ")
(display (total-weight m))

(define single 5)

(newline)
(display "Total weight of single: ")
(display (total-weight single))

; Part 3
(define (balanced? mobile)
    (define (torque branch)
        (* (branch-length branch)
           (total-weight (branch-structure branch))))

    ; A single weight is trivially balanced.
    (or (not (pair? mobile))
        ; A balanced mobile has 0 net torque, and both its branches
        ; are themselves balanced.
        (and (= (torque (left-branch mobile))
                (torque (right-branch mobile)))
             (balanced? (branch-structure (left-branch mobile)))
             (balanced? (branch-structure (right-branch mobile))))))

(define easy
    (make-mobile
        (make-branch 2 5)
        (make-branch 5 2)))

(newline)
(display "Easy is balanced? ")
(display (balanced? easy))

(define medium
    (make-mobile
        ; Total weight 7; absolute torque 21
        (make-branch 3
                     (make-mobile (make-branch 2 5)
                                  (make-branch 5 2)))
        ; Total weight 3; absolute torque 21
        (make-branch 7
                     (make-mobile (make-branch 2 1)
                                  (make-branch 1 2)))))

(newline)
(display "Medium is balanced? ")
(display (balanced? medium))

(define hard
    (make-mobile
        ; Total weight 7; absolute torque 21
        (make-branch 3
                     (make-mobile (make-branch 2 5)
                                  (make-branch 5 2)))
        ; Total weight 20; absolute torque 20
        ; Off balance by 1!
        (make-branch 1
                     (make-mobile (make-branch 2 15)
                                  (make-branch 6 5)))))

(newline)
(display "Hard is balanced? ")
(display (balanced? hard))

; Part 4

(define (make-mobile left right)
    (cons left right))

(define (make-branch length structure)
    (cons length structure))

(newline)
(display "Easy is still balanced? ")
(display (balanced? easy))

(newline)
(display "Medium is still balanced? ")
(display (balanced? medium))

(newline)
(display "Hard is still unbalanced? ")
(display (not (balanced? hard)))

