#lang planet tracer/tracer

(trace-all)

(define: (my-add [x : Number$] [y : Number$]) -> Number$
  (+ x y))

(define: x : Number$ (my-add 3 3))

(map (lambda: ([x : Number$]) -> Number$
              (my-add x 1))
     (list 2 3 4))

(define-struct: my-pair ([a : Number$] [b : Number$]))
