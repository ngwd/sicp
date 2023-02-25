#lang scheme
(display "2.38 - left right fold") (newline)
(define (fold-left op initial seq)
    (define (iter result rest)
        (if (null? rest) result
           (iter (op result (car rest)) (cdr rest))
        )
    )
    (iter initial seq)
)

(define (list-reverse seq) 
   (define (iter result rest)
      (if (null? rest) result 
          (iter (cons (car rest) result) (cdr rest))
      )
   )
   (iter '() seq)
)
(define (list-reverse2 seq)  ; use the fold-left
   (fold-left (lambda (x y)  (cons y x)) '() seq)
)

(define (accumulate op initial seq)
   (if (null? seq) initial
      (op (car seq) (accumulate op initial (cdr seq)))   
   )
)
(define fold-right accumulate)

(define (append seq1 seq2) 
    (fold-right cons seq2 seq1)
)

(append '() '((1 2) (3 4))) (newline)

(define (list-reverse3 seq)  ; use the fold-right
    ;(fold-right (lambda (x y) (append y (list x)))  '() seq)    ; also work
    (fold-right (lambda (x y) (fold-right cons (list x) y))  '() seq)
)

(list-reverse  '(1 2 3 4))
(list-reverse2 '(1 2 3 4))
(list-reverse3 '(1 2 3 4))

;(fold-right / 1 '(1 2 3)) (newline)
;(fold-left  / 1 '(1 2 3)) (newline)
;
;(fold-right / 1 '(1 2 4 8)) (newline)
;(fold-left  / 1 '(1 2 4 8)) (newline)
;
;(fold-right list '() '(1 2 3)) (newline)
;(fold-left list '() '(1 2 3))  (newline)
;
;(fold-right cons '() '(1 2 3)) (newline)
;(fold-left cons '() '(1 2 3)) (newline)

