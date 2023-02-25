 #lang sicp 
 (#%require sicp-pict) 

(define ES einstein)  
(define wave2 (beside ES (flip-vert ES)))
(paint wave2)

(define wave4 (below wave2 wave2))
(paint wave4)

(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))
(paint (flipped-pairs ES))

(define (flipped-pairs2 painter)
  (let ((painter_f (flip-vert painter)))
    (beside (below painter painter) (below painter_f painter_f))))
(paint (flipped-pairs2 ES))

(define (right-split painter n)
  (if (= n 0) painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))
(paint (right-split ES 1))
