#lang r5rs
(define (gcd a b) 
    (if (= b 0)
        a
        (gcd b (remainder a b))))

(define (rat n d) 
    (let ((g (gcd n d)))  ;; (let ((a 1) (b 2)))
    (cons (/ n g) (/ d g))
))

(define numerator car)
(define denominator cdr)
;;(define (numerator r) (car r))
;;(define (denominator r) (cdr r))
(define (print-rat r) 
   (display (car r)) (display "/") (display (cdr r))
   (newline)
)

(define r1 (rat 4 8))
(print-rat r1)
