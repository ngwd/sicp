#lang plai-typed 
(define l '(+ 1 2))
(first (s-exp->list l))
first

;; 
(define-type MisspelledAnimal
  [camal (humps : number)]
  [yacc (height : number)]
)
(define ma1 : MisspelledAnimal(camal 2))
(define ma2 : MisspelledAnimal(yacc 1.9))
(define (good? [ma : MisspelledAnimal]) : boolean
  (type-case MisspelledAnimal ma
    [camal (humps) (>= humps 2)]
    [yacc (height) (> height 2.1)]
  )
 )

(test (good? ma1) #t)
(test (good? ma2) #t)
