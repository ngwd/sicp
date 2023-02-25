#lang scheme

(define lst1 '(1 2 3 4))
(define lst2 '(5 6 7 8))
;;(list-append lst1 lst2) will yield '(1 2 3 4 5 6 7 8)
(define (list-append l1 l2) 
    (if (null? l1) 
        l2
        (cons (car l1) (list-append (cdr l1) l2))
    )
)
(define l3 (list-append lst1 lst2))
(display l3)
(newline)

(define (list-ref1 items n) 
    (if (= 0 n) 
        (car items)
        (list-ref (cdr items) (- n 1))
    )
)
;; l3 '(0 1 2 3 4)
;; (list-ref l3 0) -> 0
;; (list-ref l3 4) -> 4
;; (list-ref l3 5) -> '() 
;; (list-ref l3 -1) -> 4 
;; (list-ref l3 -5) -> 0 
;; (list-ref l3 -6) -> '() 
(define (list-ref items n)  ;; positive n
    (cond ((null? items) '()) 
          ((= 0 n) (car items))
          (else (list-ref (cdr items) (- n 1)))
    )
)
(display (list-ref l3 0)) (newline)
(display (list-ref l3 4)) (newline)
(display (list-ref l3 8)) (newline)

(define (list-length items)
    (if (null? items)
        0
        (+ 1 (list-length (cdr items)))))
(display (list-length l3)) (newline) 

;;2.17 
(define (list-tail items)
    (let ((rest (cdr items)))
        (if (null? rest)
            items
            (list-tail rest)
        )
    )
)
(display (list-tail '(1 2 3 4))) (newline)
(display (list-tail '(1))) (newline)

;;2.18  this will get (((() 3) 2) 1) 
(define (list-reverse0 items) 
    (if (null? items) '()
        (cons (list-reverse (cdr items)) (list (car items)))))

;;2.18 
(define (list-reverse2 items) 
    (define (iter-reverse lst result)
        (if (null? lst) 
            result
            (iter-reverse (cdr lst) (cons (car lst) result)))
    )
    (iter-reverse items '())
)

;;2.18 use list-append  
(define (list-reverse items) 
    (if (null? items) 
        '()
        (list-append (list-reverse (cdr items)) (list (car items)))
    )
)
(display (list-reverse '(1 2 3 4 5 6))) (newline)
;;(define (list-pair items))

;; 2.20
(define (same-parity1 x . z) ;; not x.z  but x . z
    (let ((oddity (remainder x 2)))
        (define (iter z)
            (cond ((null? z) '())
                ((= oddity (remainder (car z) 2)) (cons (car z) (iter (cdr z))))  
                (else (iter (cdr z)))
            )
        )
        (cons x (iter z))
    )
)
(define (same-parity x . z) ;; not x.z  but x . z
    (let ((parity? (if (even? x) even? odd?))) ;; 
        (define (iter z)
            (cond ((null? z) '())
                ((parity? (car z)) (cons (car z) (iter (cdr z))))  
                (else (iter (cdr z)))
            )
        )
        (cons x (iter z))
    )
)
(display (same-parity  2 3 4 5 6 7 8)) (newline)
;; 2.21
(define (square x) (* x x))
(define (square-list items) 
    (if (null? items) '()
        (cons (square (car items)) 
            (square-list (cdr items))
        )
    )
)

(define (map proc items)
    (cond ((null? items) '())
          (else                 (cons (proc (car items))     (map proc (cdr items))))
    )
)

;; tree-map 
(display "2.31")
(define (tree-map proc items)
    (cond ((null? items) '())
          ((pair? (car items))  (cons (tree-map proc (car items)) (tree-map proc (cdr items))))
          (else                 (cons (proc (car items))     (tree-map proc (cdr items))))
    )
)

(define (tree-map2 proc items)
   (lambda (subtree) 
       (if (pair? subtree) 
            (tree-map2 proc subtree)
            (proc subtree)
       )
   ) items
)


(square-list '(1 2 3 4))
(define (square-list1 items)
    (map square items)
)
(display "we are here")
(square-list1 '(1 2 3 4))


;; 2.22
(define (square-list3 items)
  (define (iter things answer) 
    (if (null? things) answer
    (iter (cdr things) (cons (square (car things)) answer))
    )
  )
  (iter items '())
)
(square-list3 '(1 2 3 4))

(define (square-list4 items)
  (define (iter things answer) 
    (if (null? things) answer
    (iter (cdr things) (cons answer (square (car things))))
    )
  )
  (iter items '())
)
(square-list4 '(1 2 3 4))

;; use list-append 
(define (square-list5 items)
  (define (iter things answer) 
    (if (null? things) answer
    (iter (cdr things) (list-append answer (list (square (car things)))))
    )
  )
  (iter items '())
)
(square-list5 '(1 2 3 4))

;; 2.23
(display "2.23 for each")
(define (for-each1 proc items) 
   (if (null? items)  true 
       (begin (proc (car items))
               (for-each1 proc (cdr items)) 
       )
   )
)
(for-each1 (lambda (x) (newline) (display x))(list 1 2 3 4))

(define (for-each2 p ls)
    (if (null? ls) true
        ((lambda (x) (p (car x)) (for-each2 p (cdr x))) ls)  ;; i.e. begin
    )
)
(for-each2 (lambda (x) (newline) (display x))(list 1 2 3 4))

;; 2.27 deep-reverse
(define (list-deep-reverse items)
   (define (iter lst result)
      (cond ((null? lst) result) 
            ((pair? (car lst)) (iter (cdr lst) (cons (list-deep-reverse (car lst)) result)))
            (else              (iter (cdr lst) (cons (car lst)                     result)))
      )
   )
   (iter items '())
)
(list-deep-reverse '(1 2 3 4))
(list-deep-reverse (list 1 (list 5 6 7) 3 (list 9 (list 11 12 13) 8)))

;; 2.28 list fringe ;; list-append used
(define (list-fringe items) 
   (if (null? items)'()
      (let ((first (car items)) (rest (cdr items)))
         (if (pair? first) 
            (list-append (list-fringe first) (list-fringe rest))
            (cons first (list-fringe rest))
         )
      )
   )
)

(define (list-fringe2 items) 
    (define (iter lst result)
        (cond ((null? lst) result)
            ((pair? (car lst)) ) 
            (else (cons (car lst) (iter items result)))
        )
    )
    (iter items '())
)
;;(list-fringe '(1 2 '(5 6) 4))
(list-fringe (list 1 2 (list 5 (list 6 8 9) 7) 4))

;; 2.30 
;; (square-tree (list 1 (list 2 (list 3 4) 5)))
(define (square-tree items)
    (map square items)    
)
(display (square-tree (list 1 (list 2 (list 3 4) 5)))) (newline)

