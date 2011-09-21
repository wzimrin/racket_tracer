#lang planet tracer/tracer

(trace-all)

(show-sharing #t)

(define: (my-add [x : Number$] [y : Number$]) -> Number$
  (+ x y))

(define: x : Number$ (my-add 3 3))

(map (lambda: ([x : Number$]) -> Number$
              (my-add x 1))
     (list 2 3 4))

(define-struct: my-pair ([a : Number$] [b : Number$]))

(define: pair1 : my-pair$ (make-my-pair 1 2))
(define: pair2 : my-pair$ pair1)

(define: (pair-first [a-pair : my-pair$]) -> Number$
  (my-pair-a a-pair))
(map pair-first (list pair1 pair2))