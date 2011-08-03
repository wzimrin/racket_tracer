#lang planet tracer/tracer

(define (fib x)
  (cond
    [(= x 0) (rest x)]
    [(= x 1) x]
    [#t (+ (fib (- x 1))
           (fib (- x 2)))]))

(fib 3)