#lang planet tracer/tracer

(check-expect (bad-fib 3) 2)

(define (bad-fib x)
  (if (<= x 1)
      x
      (+ (bad-fib (sub1 x))
         (bad-fib (sub1 x)))))
