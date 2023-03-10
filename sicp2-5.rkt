#lang scheme 
;;(define (even? n) (= 0 (bitwise-and n 1)))
(define (pow a b) 
    (cond ((= b 0) 1)
          ((even? b) (pow (* a a) (/ b 2)))
          (else  (* a (pow a (- b 1))))))
(= (pow 2 7) 128)
(define (cons x y) (* (pow 3 y) (pow 2 x)))
(= (cons 2 7) 8748)
(define (car z) 
    (let ((even-count 0))
        (define (iter t even-count)
            (if (even? t) 
                (iter (/ t 2) (+ 1 even-count))
                even-count
            )
        )
        (iter z even-count)
    )
)
(define (car0 z) 
    (let ((even-count 0))
        (define (iter t) 
            (if (odd? t) 
                even-count
                (begin ((even-count (add1 even-count)) (iter (/ t 2))))
            )
        )
        (iter z)
    )
)
(= (car (cons 2 7)) 2)
(define (cdr z) 
    (let ((3-count 0))
        (define (iter t 3-count)
            (if (= 0 (remainder t 3))
                (iter (/ t 3) (add1 3-count))
                3-count
            )
        )
        (iter z 3-count)
    )
)
(= (cdr (cons 2 7)) 7)
;;(= (cdr 8748) 7)