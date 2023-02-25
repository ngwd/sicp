#lang scheme
(display "2.33") (newline)
(define (filter predicate items)
    (cond ((null? items) '())
        ((predicate (car items)) (cons (car items) (filter predicate (cdr items))))
        (else (filter predicate (cdr items)))
    )
)
;(filter odd? '(1 2 3 4 5 6 7 8))
(define (accumulate op initial sequence)
    (if (null? sequence) initial
        ;(accumulate op (op initial (car sequence)) (cdr sequence))
        ;(accumulate op (op (car sequence) initial) (cdr sequence))
        (op (car sequence) 
            (accumulate op initial (cdr sequence)))
    )
)
(accumulate + 0 '(1 2 3 4 5 6))
(accumulate * 1 '(1 2 3 4 5 6))
(accumulate cons '() '(1 2 3 4 5 6))
(display "---3---") (newline)
(define (length sequence)
     (accumulate  (lambda (x y) (+ 1 y)) 0 sequence)
) 
(length '(1 2 3 4 5))

(display "---1---") (newline)
(define (map1 p sequence) 
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence)
)
(map1 add1 '(1 2 3 4))

(display "---2---") (newline)
(define (append seq1 seq2) 
    (accumulate cons seq2 seq1)
)
(append '(1 2 3 4)  '(5 6 7 8))

(define (accumulate-fake op initial sequence)
    (if (null? sequence) initial
        (op (car sequence) 
            (accumulate op initial (cdr sequence)))
    )
)
(display "2.34 - Horner Eval") (newline)
(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (u v) (+ u (* x v))) 0 coefficient-sequence)
)
; to get 1 + 3x + 5x^3 + x^5 value at x = 2
(horner-eval 2 '(1 3 0 5 0 1))

(define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
                   (count-leaves (cdr x))))
    )
)

(count-leaves `(1 `(2 `(3 4))))  ; get 6 
(count-leaves `(1 (2 (3 4))))    ; get 4 
(count-leaves (list 1 (list 2 (list 3 4)))) ; get 4

(display "2.35 - Count leaves in accumulate manner") (newline)
(define (count-leaves2 seq)
   (let ((op (lambda (x) (cond ((null? x) 0)
                        ((pair? x) (count-leaves2 x))
                        (else 1)))))
   (accumulate  (lambda (u v) (+ (op u) v))  0  seq))
)
(count-leaves2 `(1 (2 (3 4) (5 6)) (7 8)))    ; get 8 
(display "2.36 - accumulate-n ") (newline)
;seq     (( 1  2  3)
;         ( 4  5  6)
;         ( 7  8  9)
;         (10 11 12))
; (accumulate-n + 0 seq) will get (22, 26 30)

(define (acc op initial seq)
   (if (null? seq) initial
       (op (car seq) (acc op initial (cdr seq)))
   )
)
(define (car-c seqs) ; column-wise car 
   (accumulate (lambda (x y) (cons (car x) y)) '() seqs)
)
(define (cdr-c seqs) ; column-wise cdr
   (accumulate (lambda (x y) (cons (cdr x) y)) '() seqs)
)
(let ((matrix '((1 2 3) (4 5 6) (7 8 9))))
   (display matrix) 
   (car-c matrix) 
   (cdr-c matrix) 
)(newline)

(define (accumulate-n op init seqs)
    (if (null? (car seqs)) '()
        (cons (accumulate op init   (car-c seqs))
              (accumulate-n op init (cdr-c seqs))))
)

(let ((matrix '((1 2 3) (4 5 6) (7 8 9))))
   (accumulate-n + 0 matrix)
)(newline)

(display "2.37 - matrix operations ") (newline)
(define (dot-product v w) (accumulate + 0 (map * v w)))
(display (dot-product '(1 2 3) '(4 5 6))) (newline)

(define (matrix-*-vector m v) (map (lambda (x) (dot-product x v)) m))
(matrix-*-vector '((1 2 3) (4 5 6) (7 8 9)) '(3 2 1)) (newline)

(define (transpose m) (accumulate-n cons '() m))
(transpose '((1 2 3) (4 5 6) (7 8 9))) (newline)

(define (matrix-*-matrix m n)
   (let ((cols (transpose n)))
      (map (lambda (x) (matrix-*-vector cols x)) m)
   )
)
(matrix-*-matrix '((1 2 3) (3 4 1)) '((1 2) (3 4) (1 1))) (newline)
