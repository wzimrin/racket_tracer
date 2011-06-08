#lang s-exp "tracer.rkt"

(require net/sendurl)

(define (fib x)
  (if (< x 2)
      x
      (+ (fib (- x 1))
         (fib (- x 2)))))

(define (squared x)
  (* x x))

(define (dist x1 y1 x2 y2)
  (sqrt (+ (squared (- x1 x2))
           (squared (- y1 y2)))))

(define (close-enough? x y)
  (< (dist x y 0 0) 4))

(fib 10)
(close-enough? 3 3)

(define (all-close-enough? xs)
  (andmap (lambda (x)
            (close-enough? (first x) (second x)))
          xs))

(all-close-enough? '((1 1) (2 2) (3 3) (4 4)))

(trace->json)
(send-url "index.html")