#lang planet tracer/tracer


(define (bad-fib x)
  (if (<= x 1)
      x
      (+ (bad-fib (sub1 x))
         (bad-fib (sub1 x)))))


(bad-fib 3)
(bad-fib 2)
(check-expect (bad-fib 3) 2)
(check-expect (bad-fib 1) 17)
