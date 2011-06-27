#lang planet tracer/tracer

(define (my-id x) x)
(define (my-add a b) (+ a b))

(check-expect (my-add 3 4) (my-add 4 3))

;dependent bindings in let
(let* ([x 5]
      [y (+ x 3)])
  (my-add x y))

;function binding in let
(let ([n (λ(a) (* 3 a))])
  (n 4))

;Don't lose calls to my-id
(map my-id
     (build-list 10 (λ(n) n)))