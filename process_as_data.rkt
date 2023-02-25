#lang r5rs
;; (car(cons exp_car exp_cdr)) == exp_car
;; (cdr(cons exp_car exp_cdr)) == exp_cdr

(define (conss x y) (lambda (m) (m x y)))
(define (carr z) (z (lambda (p q) p)))
(define ccc (carr (conss 'a 'b)))
(display ccc) ;; 'a
