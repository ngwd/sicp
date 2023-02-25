#lang scheme

(define (list-append l1 l2) 
    (if (null? l1) 
        l2
        (cons (car l1) (list-append (cdr l1) l2))
    )
)

;; map now deals with tree
;;(define (map proc items)
;;    (cond ((null? items) '())
;;          ((null? (car items))  (cons '()                    (map proc (cdr items))))
;;          ((pair? (car items))  (cons (map proc (car items)) (map proc (cdr items))))
;;          (else                 (cons (proc (car items))     (map proc (cdr items))))
;;    )
;;)

(define (map proc items)
    (cond ((null? items) '())
          (else                 (cons (proc (car items))     (map proc (cdr items))))
    )
)

;; 2.32
;; {1 2 3} subsets are {{} {2} {3} {2 3} {1} {1 3} {1 2} {1 2 3}}
(display "2.32")(newline)
(define (subsets s) 
    (if (null? s) '(())
        (let ((rest (subsets (cdr s))))
            (list-append rest (map (lambda (x) (cons (car s) x)) rest))
            ;;(append rest (map (lambda (x) (append x (list (car s))))  rest))
        )
    )
)
;; (display (subsets (list 1 2 3 4 5 6 7 8)))
(display (subsets (list 3 2 1))) (newline)
