#lang planet tracer/tracer

(define (foo a b c d e f g)
  (if g
      (foo a b c d e f false)
      "HULLABALLOOOOO"))

(foo 1
     "hello"
     (lambda(x) (* x x x x))
     (list (list 3 4) 5 7 
           (list (list 2)))
     (exp 20)
     "hi hi hi hi hi hi hi hi hi"
     true)