#lang planet tracer/tracer

(trace-all)
(define (fib x)
  (cond
    [(= x 0) (rest x)]
    [(= x 1) x]
    [#t (+ (fib (- x 1))
           (fib (- x 2)))]))

(fib 2)
;(check-expect (fib 2) 1)

#|
(check-within (fib 1) 0 1)
(check-within (fib 1) 0 0)

(check-error (fib 1))

(check-expect (fib 1) 2)

(check-member-of 1 1 2 3 4)
(check-member-of 5 1 2 3 4)

(check-range 3 2 5)
(check-range 2 3 5)
(check-range 5 2 3)
|#